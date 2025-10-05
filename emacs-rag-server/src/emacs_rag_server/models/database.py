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

    # Create FTS5 virtual table for full-text search
    client.execute("""
        CREATE VIRTUAL TABLE IF NOT EXISTS documents_fts USING fts5(
            id UNINDEXED,
            source_path,
            content,
            content=documents,
            content_rowid=rowid
        )
    """)

    # Create org headings table for fast heading navigation
    client.execute("""
        CREATE TABLE IF NOT EXISTS org_headings (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            source_path TEXT NOT NULL,
            line_number INTEGER NOT NULL,
            heading_text TEXT NOT NULL,
            tags TEXT,
            level INTEGER NOT NULL,
            created_at INTEGER DEFAULT (strftime('%s', 'now')),
            UNIQUE(source_path, line_number)
        )
    """)

    # Create index on source_path for faster lookups
    client.execute(
        "CREATE INDEX IF NOT EXISTS idx_org_headings_path ON org_headings(source_path)"
    )

    # Create org heading embeddings table for semantic search
    client.execute("""
        CREATE TABLE IF NOT EXISTS org_heading_embeddings (
            heading_id INTEGER PRIMARY KEY,
            vector BLOB NOT NULL,
            model TEXT NOT NULL,
            created_at INTEGER DEFAULT (strftime('%s', 'now')),
            FOREIGN KEY (heading_id) REFERENCES org_headings(id) ON DELETE CASCADE
        )
    """)

    # Create vector index for org heading cosine similarity search
    try:
        client.execute(
            "CREATE INDEX IF NOT EXISTS idx_org_heading_embeddings_vector ON org_heading_embeddings(vector) USING vector_cosine"
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

        # Insert into FTS5 table
        client.execute("""
            INSERT OR REPLACE INTO documents_fts (id, source_path, content)
            VALUES (?, ?, ?)
        """, [doc_id, source_path, content])


def delete_documents_for_path(path: str) -> None:
    """
    Delete all chunks for a file.

    Args:
        path: Absolute file path
    """
    client = get_client()
    client.execute("DELETE FROM documents WHERE source_path = ?", [path])
    client.execute("DELETE FROM documents_fts WHERE source_path = ?", [path])
    client.execute("DELETE FROM org_headings WHERE source_path = ?", [path])


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


def get_all_indexed_files() -> List[str]:
    """Get list of all unique file paths in the database."""
    client = get_client()
    result = client.execute("SELECT DISTINCT source_path FROM documents ORDER BY source_path")
    return [row[0] for row in result.rows]


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


def query_hybrid(query: str, query_embedding: List[float], *, n_results: int = 5, vector_weight: float = 0.5) -> Dict[str, Any]:
    """
    Perform hybrid search combining vector similarity and full-text search.

    Args:
        query: Text query for FTS5 search
        query_embedding: Query embedding vector for similarity search
        n_results: Maximum number of results to return
        vector_weight: Weight for vector scores (0-1), text weight is (1-vector_weight)

    Returns:
        Dictionary with keys: ids, documents, metadatas, distances (combined scores)
        Format matches ChromaDB for compatibility
    """
    client = get_client()
    vector_bytes = struct.pack(f'{len(query_embedding)}f', *query_embedding)

    # Get vector search results
    try:
        vector_result = client.execute("""
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
        """, [vector_bytes, n_results * 3])  # Get more candidates for fusion
    except Exception:
        # Fallback without vector extension
        vector_result = client.execute("""
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
            LIMIT ?
        """, [n_results * 3])

    # Get FTS5 results
    fts_result = client.execute("""
        SELECT
            d.id,
            d.source_path,
            d.chunk_index,
            d.line_number,
            d.content,
            d.chunk_size,
            d.chunk_total,
            d.metadata,
            fts.rank,
            snippet(documents_fts, 2, '<<<', '>>>', '...', 15) as snippet
        FROM documents_fts fts
        JOIN documents d ON fts.id = d.id
        WHERE documents_fts MATCH ?
        ORDER BY rank
        LIMIT ?
    """, [query, n_results * 3])

    # Normalize and combine scores
    vector_scores = {}
    fts_scores = {}

    # Process vector results
    if hasattr(vector_result.rows[0], '__len__') and len(vector_result.rows) > 0:
        for row in vector_result.rows:
            doc_id = row[0]
            if len(row) == 9 and isinstance(row[8], (int, float)):
                # Has distance from vector_distance_cosine
                distance = row[8]
                vector_scores[doc_id] = 1.0 - distance  # Convert to similarity
            else:
                # Fallback: calculate manually
                stored_vector = struct.unpack(f'{len(query_embedding)}f', row[8])
                dot_product = sum(a * b for a, b in zip(query_embedding, stored_vector))
                vector_scores[doc_id] = dot_product

    # Process FTS5 results
    for row in fts_result.rows:
        doc_id = row[0]
        rank = -row[8] if row[8] is not None else 0.0  # Convert negative rank to positive
        fts_scores[doc_id] = rank

    # Normalize scores to 0-1 range
    if vector_scores:
        max_vec = max(vector_scores.values())
        min_vec = min(vector_scores.values())
        if max_vec > min_vec:
            vector_scores = {k: (v - min_vec) / (max_vec - min_vec) for k, v in vector_scores.items()}

    if fts_scores:
        max_fts = max(fts_scores.values())
        min_fts = min(fts_scores.values())
        if max_fts > min_fts:
            fts_scores = {k: (v - min_fts) / (max_fts - min_fts) for k, v in fts_scores.items()}

    # Combine scores
    all_doc_ids = set(vector_scores.keys()) | set(fts_scores.keys())
    combined_scores = {}
    for doc_id in all_doc_ids:
        vec_score = vector_scores.get(doc_id, 0.0)
        fts_score = fts_scores.get(doc_id, 0.0)
        combined_scores[doc_id] = (vector_weight * vec_score) + ((1 - vector_weight) * fts_score)

    # Sort by combined score and take top N
    top_ids = sorted(combined_scores.keys(), key=lambda x: combined_scores[x], reverse=True)[:n_results]

    # Fetch full details for top results
    if not top_ids:
        return {'ids': [[]], 'documents': [[]], 'metadatas': [[]], 'distances': [[]]}

    placeholders = ','.join('?' * len(top_ids))
    final_result = client.execute(f"""
        SELECT
            d.id,
            d.source_path,
            d.chunk_index,
            d.line_number,
            d.content,
            d.chunk_size,
            d.chunk_total,
            d.metadata
        FROM documents d
        WHERE d.id IN ({placeholders})
    """, top_ids)

    # Format results maintaining score order
    id_to_row = {row[0]: row for row in final_result.rows}
    ids = []
    documents = []
    metadatas = []
    distances = []

    for doc_id in top_ids:
        if doc_id not in id_to_row:
            continue
        row = id_to_row[doc_id]

        ids.append(row[0])
        documents.append(row[4])

        base_metadata = {
            'source_path': row[1],
            'chunk_index': row[2],
            'line_number': row[3],
            'chunk_size': row[5],
            'chunk_total': row[6]
        }

        if row[7]:
            extra = json.loads(row[7])
            base_metadata.update(extra)

        metadatas.append(base_metadata)
        # Use inverse of combined score as distance (lower is better)
        distances.append(1.0 - combined_scores[doc_id])

    return {
        'ids': [ids],
        'documents': [documents],
        'metadatas': [metadatas],
        'distances': [distances]
    }


def query_by_text(query: str, *, n_results: int = 5) -> Dict[str, Any]:
    """
    Perform full-text search using FTS5.

    Args:
        query: Text query to search for
        n_results: Maximum number of results to return

    Returns:
        Dictionary with keys: ids, documents, metadatas, distances (BM25 scores)
        Format matches ChromaDB for compatibility
    """
    client = get_client()

    # Perform FTS5 search with BM25 ranking and snippet extraction
    # snippet() parameters: column, start_tag, end_tag, ellipsis, num_tokens
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
            fts.rank,
            snippet(documents_fts, 2, '<<<', '>>>', '...', 15) as snippet
        FROM documents_fts fts
        JOIN documents d ON fts.id = d.id
        WHERE documents_fts MATCH ?
        ORDER BY rank
        LIMIT ?
    """, [query, n_results])

    # Format results
    ids = []
    documents = []
    metadatas = []
    distances = []

    for row in result.rows:
        # row is tuple: (id, source_path, chunk_index, line_number, content, chunk_size, chunk_total, metadata, rank, snippet)
        ids.append(row[0])
        # Use snippet instead of full content
        documents.append(row[9])  # snippet is at index 9

        # Reconstruct metadata
        base_metadata = {
            'source_path': row[1],
            'chunk_index': row[2],
            'line_number': row[3],
            'chunk_size': row[5],
            'chunk_total': row[6]
        }

        # Add extra metadata if present
        if row[7]:
            extra = json.loads(row[7])
            base_metadata.update(extra)

        metadatas.append(base_metadata)
        # FTS5 rank is negative, convert to positive distance for consistency
        distances.append(-row[8] if row[8] is not None else 0.0)

    return {
        'ids': [ids],
        'documents': [documents],
        'metadatas': [metadatas],
        'distances': [distances]
    }


def rebuild_fts_index() -> int:
    """
    Rebuild the FTS5 index from the documents table.

    This is useful when the FTS5 table gets out of sync with the main documents table.

    Returns:
        Number of documents reindexed
    """
    client = get_client()

    # Drop and recreate the FTS5 table
    client.execute("DROP TABLE IF EXISTS documents_fts")

    client.execute("""
        CREATE VIRTUAL TABLE documents_fts USING fts5(
            id UNINDEXED,
            source_path,
            content,
            content=documents,
            content_rowid=rowid
        )
    """)

    # Populate FTS5 table from existing documents
    client.execute("""
        INSERT INTO documents_fts (id, source_path, content)
        SELECT id, source_path, content FROM documents
    """)

    # Get count of documents
    result = client.execute("SELECT COUNT(*) FROM documents_fts")
    count = result.rows[0][0] if result.rows else 0

    return count


def add_org_headings(path: str, content: str) -> int:
    """
    Extract and store org headings from file content with embeddings.

    Args:
        path: Absolute file path
        content: File content

    Returns:
        Number of headings extracted
    """
    import re

    if not path.endswith('.org'):
        return 0

    client = get_client()
    settings = get_settings()

    # Import embedding model (lazy import to avoid circular dependency)
    from ..models.embeddings import get_embedding_model

    # Delete existing headings for this file
    client.execute("DELETE FROM org_headings WHERE source_path = ?", [path])

    # Parse org headings
    heading_pattern = re.compile(r'^(\*+)\s+(.+?)(?:\s+(:[a-zA-Z0-9_@:]+:))?\s*$')
    lines = content.split('\n')
    headings_data = []

    for line_num, line in enumerate(lines, 1):
        match = heading_pattern.match(line)
        if match:
            stars = match.group(1)
            heading_text = match.group(2).strip()
            tags = match.group(3).strip() if match.group(3) else None
            level = len(stars)

            headings_data.append({
                'line_num': line_num,
                'heading_text': heading_text,
                'tags': tags,
                'level': level
            })

    if not headings_data:
        return 0

    # Insert headings and get their IDs
    embedding_model = get_embedding_model()
    heading_texts = []
    heading_ids = []

    for heading in headings_data:
        # Insert heading and get the ID
        result = client.execute("""
            INSERT INTO org_headings
            (source_path, line_number, heading_text, tags, level)
            VALUES (?, ?, ?, ?, ?)
            RETURNING id
        """, [path, heading['line_num'], heading['heading_text'],
              heading['tags'], heading['level']])

        heading_id = result.rows[0][0]
        heading_ids.append(heading_id)

        # Prepare text for embedding: combine heading text with tags if present
        text_to_embed = heading['heading_text']
        if heading['tags']:
            text_to_embed = f"{heading['heading_text']} {heading['tags']}"
        heading_texts.append(text_to_embed)

    # Generate embeddings in batch
    embeddings = embedding_model.embed_documents(heading_texts)

    # Store embeddings
    for heading_id, embedding in zip(heading_ids, embeddings):
        vector_bytes = struct.pack(f'{len(embedding)}f', *embedding)
        client.execute("""
            INSERT INTO org_heading_embeddings (heading_id, vector, model)
            VALUES (?, ?, ?)
        """, [heading_id, vector_bytes, settings.embedding_model])

    return len(headings_data)


def get_all_org_headings() -> List[Dict[str, Any]]:
    """
    Get all org headings from the database.

    Returns:
        List of dictionaries with heading information
    """
    client = get_client()
    result = client.execute("""
        SELECT source_path, line_number, heading_text, tags, level
        FROM org_headings
        ORDER BY source_path, line_number
    """)

    headings = []
    for row in result.rows:
        headings.append({
            'source_path': row[0],
            'line_number': row[1],
            'heading_text': row[2],
            'tags': row[3],
            'level': row[4]
        })

    return headings


def query_org_headings_by_vector(query_embedding: List[float], *, n_results: int = 20) -> List[Dict[str, Any]]:
    """
    Perform vector similarity search on org headings.

    Args:
        query_embedding: Query embedding vector
        n_results: Maximum number of results to return

    Returns:
        List of dictionaries with heading information and similarity scores
    """
    client = get_client()
    vector_bytes = struct.pack(f'{len(query_embedding)}f', *query_embedding)

    try:
        # Try using vector_distance_cosine function if available
        result = client.execute("""
            SELECT
                h.source_path,
                h.line_number,
                h.heading_text,
                h.tags,
                h.level,
                vector_distance_cosine(e.vector, ?) as distance
            FROM org_headings h
            JOIN org_heading_embeddings e ON h.id = e.heading_id
            ORDER BY distance ASC
            LIMIT ?
        """, [vector_bytes, n_results])

        # Format results
        headings = []
        for row in result.rows:
            headings.append({
                'source_path': row[0],
                'line_number': row[1],
                'heading_text': row[2],
                'tags': row[3],
                'level': row[4],
                'score': 1.0 - row[5]  # Convert distance to similarity score
            })

    except Exception:
        # Fallback: manual cosine distance calculation
        result = client.execute("""
            SELECT
                h.source_path,
                h.line_number,
                h.heading_text,
                h.tags,
                h.level,
                e.vector
            FROM org_headings h
            JOIN org_heading_embeddings e ON h.id = e.heading_id
        """)

        # Calculate cosine distances in Python
        headings_with_distance = []
        for row in result.rows:
            stored_vector = struct.unpack(f'{len(query_embedding)}f', row[5])
            # Cosine similarity (assuming normalized vectors)
            dot_product = sum(a * b for a, b in zip(query_embedding, stored_vector))
            similarity = dot_product

            headings_with_distance.append({
                'source_path': row[0],
                'line_number': row[1],
                'heading_text': row[2],
                'tags': row[3],
                'level': row[4],
                'score': similarity
            })

        # Sort by similarity (descending) and limit
        headings_with_distance.sort(key=lambda x: x['score'], reverse=True)
        headings = headings_with_distance[:n_results]

    return headings
