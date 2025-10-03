"""LibSQL database interface for vector storage and retrieval."""

import json
import struct
from functools import lru_cache
from typing import Any, Dict, List

import libsql_client

from ..utils.config import get_settings


@lru_cache
def get_client() -> libsql_client.Client:
    """Get cached LibSQL client instance."""
    settings = get_settings()
    db_file = settings.db_path / "rag.db"
    return libsql_client.create_client_sync(
        url=f"file:{db_file}"
    )


def init_schema() -> None:
    """Initialize database schema if tables don't exist."""
    client = get_client()

    # Create documents table
    client.execute("""
        CREATE TABLE IF NOT EXISTS documents (
            id TEXT PRIMARY KEY,
            source_path TEXT NOT NULL,
            chunk_index INTEGER NOT NULL,
            line_number INTEGER NOT NULL,
            content TEXT NOT NULL,
            chunk_size INTEGER NOT NULL,
            chunk_total INTEGER NOT NULL,
            metadata JSON,
            created_at INTEGER DEFAULT (strftime('%s', 'now')),
            updated_at INTEGER DEFAULT (strftime('%s', 'now'))
        )
    """)

    # Create embeddings table
    client.execute("""
        CREATE TABLE IF NOT EXISTS embeddings (
            id TEXT PRIMARY KEY,
            vector BLOB NOT NULL,
            model TEXT NOT NULL,
            created_at INTEGER DEFAULT (strftime('%s', 'now')),
            FOREIGN KEY (id) REFERENCES documents(id) ON DELETE CASCADE
        )
    """)

    # Create indexes
    client.execute(
        "CREATE INDEX IF NOT EXISTS idx_documents_path ON documents(source_path)"
    )
    client.execute(
        "CREATE INDEX IF NOT EXISTS idx_documents_chunk ON documents(source_path, chunk_index)"
    )

    # Create vector index for cosine similarity search
    try:
        client.execute(
            "CREATE INDEX IF NOT EXISTS idx_embeddings_vector ON embeddings(vector) USING vector_cosine"
        )
    except Exception:
        # Vector extension may not be available, index will be created without optimization
        pass


def add_documents(
    *,
    ids: List[str],
    documents: List[str],
    metadatas: List[Dict[str, Any]],
    embeddings: List[List[float]]
) -> None:
    """
    Insert documents and embeddings.

    Args:
        ids: List of document IDs (format: {path}:{chunk_index})
        documents: List of document text chunks
        metadatas: List of metadata dictionaries
        embeddings: List of embedding vectors
    """
    client = get_client()
    settings = get_settings()

    for doc_id, content, metadata, embedding in zip(ids, documents, metadatas, embeddings):
        # Convert embedding to bytes (float32 array)
        vector_bytes = struct.pack(f'{len(embedding)}f', *embedding)

        # Extract standard metadata fields
        source_path = metadata['source_path']
        chunk_index = metadata['chunk_index']
        line_number = metadata['line_number']
        chunk_size = metadata['chunk_size']
        chunk_total = metadata['chunk_total']

        # Remaining metadata as JSON
        extra_metadata = {
            k: v for k, v in metadata.items()
            if k not in ['source_path', 'chunk_index', 'line_number', 'chunk_size', 'chunk_total']
        }
        metadata_json = json.dumps(extra_metadata) if extra_metadata else None

        # Insert document
        client.execute("""
            INSERT OR REPLACE INTO documents
            (id, source_path, chunk_index, line_number, content, chunk_size, chunk_total, metadata)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        """, [
            doc_id,
            source_path,
            chunk_index,
            line_number,
            content,
            chunk_size,
            chunk_total,
            metadata_json
        ])

        # Insert embedding
        client.execute("""
            INSERT OR REPLACE INTO embeddings (id, vector, model)
            VALUES (?, ?, ?)
        """, [doc_id, vector_bytes, settings.embedding_model])


def delete_documents_for_path(path: str) -> None:
    """
    Delete all chunks for a file.

    Args:
        path: Absolute file path
    """
    client = get_client()
    client.execute("DELETE FROM documents WHERE source_path = ?", [path])


def query_by_vector(query_embedding: List[float], *, n_results: int = 5) -> Dict[str, Any]:
    """
    Perform vector similarity search.

    Args:
        query_embedding: Query embedding vector
        n_results: Maximum number of results to return

    Returns:
        Dictionary with keys: ids, documents, metadatas, distances
        Format matches ChromaDB for compatibility
    """
    client = get_client()
    vector_bytes = struct.pack(f'{len(query_embedding)}f', *query_embedding)

    try:
        # Try using vector_distance_cosine function if available
        result = client.execute("""
            SELECT
                d.id,
                d.source_path,
                d.chunk_index,
                d.line_number,
                d.content,
                d.chunk_size,
                d.chunk_total,
                d.metadata,
                vector_distance_cosine(e.vector, ?) as distance
            FROM documents d
            JOIN embeddings e ON d.id = e.id
            ORDER BY distance ASC
            LIMIT ?
        """, [vector_bytes, n_results])

        # Format results - row is tuple: (id, source_path, chunk_index, line_number, content, chunk_size, chunk_total, metadata, distance)
        formatted_rows = []
        for row in result.rows:
            formatted_rows.append({
                'id': row[0],
                'source_path': row[1],
                'chunk_index': row[2],
                'line_number': row[3],
                'content': row[4],
                'chunk_size': row[5],
                'chunk_total': row[6],
                'metadata': row[7],
                'distance': row[8]
            })
        result_rows = formatted_rows

    except Exception:
        # Fallback: manual cosine distance calculation
        # This is slower but works without vector extension
        result = client.execute("""
            SELECT
                d.id,
                d.source_path,
                d.chunk_index,
                d.line_number,
                d.content,
                d.chunk_size,
                d.chunk_total,
                d.metadata,
                e.vector
            FROM documents d
            JOIN embeddings e ON d.id = e.id
        """)

        # Calculate cosine distances in Python
        rows_with_distance = []
        for row in result.rows:
            # row is tuple: (id, source_path, chunk_index, line_number, content, chunk_size, chunk_total, metadata, vector)
            stored_vector = struct.unpack(f'{len(query_embedding)}f', row[8])  # vector is at index 8
            # Cosine distance = 1 - cosine similarity
            dot_product = sum(a * b for a, b in zip(query_embedding, stored_vector))
            distance = 1.0 - dot_product  # Assuming normalized vectors
            rows_with_distance.append({
                'id': row[0],
                'source_path': row[1],
                'chunk_index': row[2],
                'line_number': row[3],
                'content': row[4],
                'chunk_size': row[5],
                'chunk_total': row[6],
                'metadata': row[7],
                'distance': distance
            })

        # Sort by distance and limit
        rows_with_distance.sort(key=lambda x: x['distance'])
        result_rows = rows_with_distance[:n_results]

    # Format results to match ChromaDB structure
    ids = []
    documents = []
    metadatas = []
    distances = []

    for row in result_rows:
        ids.append(row['id'])
        documents.append(row['content'])

        # Reconstruct metadata
        base_metadata = {
            'source_path': row['source_path'],
            'chunk_index': row['chunk_index'],
            'line_number': row['line_number'],
            'chunk_size': row['chunk_size'],
            'chunk_total': row['chunk_total']
        }

        # Add extra metadata if present
        if row.get('metadata') and row['metadata']:
            extra = json.loads(row['metadata'])
            base_metadata.update(extra)

        metadatas.append(base_metadata)
        distances.append(row['distance'])

    return {
        'ids': [ids],
        'documents': [documents],
        'metadatas': [metadatas],
        'distances': [distances]
    }


def get_total_chunks() -> int:
    """Get total number of chunks in the database."""
    client = get_client()
    result = client.execute("SELECT COUNT(*) as count FROM documents")
    return result.rows[0][0] if result.rows else 0


def get_total_unique_files() -> int:
    """Get total number of unique files in the database."""
    client = get_client()
    result = client.execute("SELECT COUNT(DISTINCT source_path) as count FROM documents")
    return result.rows[0][0] if result.rows else 0


def get_sample_chunk() -> Dict[str, Any] | None:
    """Get a sample chunk for inspection."""
    client = get_client()
    result = client.execute("""
        SELECT id, source_path, chunk_index, line_number, content, chunk_size, chunk_total
        FROM documents LIMIT 1
    """)

    if not result.rows:
        return None

    row = result.rows[0]
    # row is a tuple: (id, source_path, chunk_index, line_number, content, chunk_size, chunk_total)
    return {
        'ids': row[0],  # id
        'documents': row[4],  # content
        'metadatas': {
            'source_path': row[1],  # source_path
            'chunk_index': row[2],  # chunk_index
            'line_number': row[3],  # line_number
            'chunk_size': row[5],  # chunk_size
            'chunk_total': row[6]  # chunk_total
        }
    }
