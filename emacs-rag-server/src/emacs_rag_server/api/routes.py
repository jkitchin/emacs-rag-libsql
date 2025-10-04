"""FastAPI route definitions."""

from typing import Optional

from fastapi import APIRouter, HTTPException, Query
from fastapi.responses import HTMLResponse

from ..models.schemas import (
    DeleteResponse,
    HealthResponse,
    IndexedFilesResponse,
    IndexRequest,
    IndexResponse,
    RebuildFtsResponse,
    SearchResponse,
    SearchResult,
    StatsResponse,
)
from ..models.database import get_all_indexed_files, rebuild_fts_index
from ..services.file_service import delete_file, index_file
from ..services.search_service import hybrid_search, text_search, vector_search
from ..services.stats_service import database_stats
from ..utils.config import get_settings

router = APIRouter()


@router.get("/", response_class=HTMLResponse)
async def home():
    """HTML landing page with server information."""
    settings = get_settings()

    html_content = f"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>Emacs RAG Server</title>
        <style>
            body {{
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                max-width: 800px;
                margin: 50px auto;
                padding: 20px;
                line-height: 1.6;
            }}
            h1 {{ color: #333; }}
            h2 {{ color: #555; margin-top: 30px; }}
            .config {{ background: #f5f5f5; padding: 15px; border-radius: 5px; }}
            .endpoint {{ background: #e8f4f8; padding: 10px; margin: 10px 0; border-radius: 3px; }}
            code {{ background: #eee; padding: 2px 6px; border-radius: 3px; }}
            a {{ color: #0066cc; text-decoration: none; }}
            a:hover {{ text-decoration: underline; }}
        </style>
    </head>
    <body>
        <h1>Emacs RAG Server</h1>
        <p>FastAPI server with LibSQL backend for semantic search and retrieval-augmented generation.</p>

        <h2>Server Status</h2>
        <p>✓ Running on {settings.host}:{settings.port}</p>

        <h2>Configuration</h2>
        <div class="config">
            <p><strong>Database:</strong> {settings.db_path}/rag.db</p>
            <p><strong>Embedding Model:</strong> {settings.embedding_model}</p>
            <p><strong>Reranking Model:</strong> {settings.rerank_model}</p>
            <p><strong>Reranking Enabled:</strong> {settings.rerank_enabled}</p>
            <p><strong>Chunk Size:</strong> {settings.chunk_size} chars</p>
            <p><strong>Chunk Overlap:</strong> {settings.chunk_overlap} chars</p>
        </div>

        <h2>Available Endpoints</h2>

        <div class="endpoint">
            <strong>POST /index</strong> - Index a file with automatic chunking and embedding
        </div>

        <div class="endpoint">
            <strong>GET /search/vector</strong> - Semantic similarity search
        </div>

        <div class="endpoint">
            <strong>GET /search/text</strong> - Full-text search using FTS5
        </div>

        <div class="endpoint">
            <strong>GET /search/hybrid</strong> - Hybrid search combining vector and full-text
        </div>

        <div class="endpoint">
            <strong>DELETE /files</strong> - Remove all chunks for a file
        </div>

        <div class="endpoint">
            <strong>GET /stats</strong> - Database statistics
        </div>

        <div class="endpoint">
            <strong>GET /health</strong> - Health check
        </div>

        <div class="endpoint">
            <strong>POST /rebuild-fts</strong> - Rebuild FTS5 index from documents
        </div>

        <div class="endpoint">
            <strong>GET /files</strong> - List all indexed files
        </div>

        <h2>Documentation</h2>
        <p>
            <a href="/docs">Interactive API Documentation (Swagger UI)</a><br>
            <a href="/redoc">Alternative Documentation (ReDoc)</a>
        </p>

        <h2>Quick Links</h2>
        <p>
            <a href="/health">Health Check</a> |
            <a href="/stats">Statistics</a>
        </p>
    </body>
    </html>
    """
    return html_content


@router.post("/index", response_model=IndexResponse)
async def index_endpoint(request: IndexRequest):
    """
    Index or update a file with automatic chunking and embedding.

    - **path**: Absolute file path
    - **content**: Optional content override (if not provided, reads from file)
    - **metadata**: Optional custom metadata to attach to chunks

    Returns the file path and number of chunks indexed.
    """
    try:
        resolved_path, chunks_indexed = index_file(
            request.path,
            content=request.content,
            metadata=request.metadata
        )
        return IndexResponse(
            path=resolved_path,
            chunks_indexed=chunks_indexed
        )
    except FileNotFoundError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except PermissionError as e:
        raise HTTPException(status_code=403, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Indexing failed: {str(e)}")


@router.get("/search/vector", response_model=SearchResponse)
async def search_vector_endpoint(
    query: str = Query(..., description="Search query text"),
    limit: int = Query(5, ge=1, le=100, description="Maximum number of results"),
    rerank: bool = Query(True, description="Enable reranking")
):
    """
    Semantic similarity search using embeddings.

    - **query**: Search query text
    - **limit**: Maximum number of results (1-100)
    - **rerank**: Whether to apply two-stage reranking

    Returns list of matching chunks with scores.
    """
    try:
        results = vector_search(query, limit=limit, rerank=rerank)
        search_results = [SearchResult(**r) for r in results]
        return SearchResponse(results=search_results)
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Search failed: {str(e)}")


@router.get("/search/text", response_model=SearchResponse)
async def search_text_endpoint(
    query: str = Query(..., description="Full-text search query"),
    limit: int = Query(5, ge=1, le=100, description="Maximum number of results")
):
    """
    Full-text search using FTS5 with BM25 ranking.

    - **query**: Full-text search query (supports FTS5 syntax)
    - **limit**: Maximum number of results (1-100)

    Returns list of matching chunks with BM25 scores.
    """
    try:
        results = text_search(query, limit=limit)
        search_results = [SearchResult(**r) for r in results]
        return SearchResponse(results=search_results)
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Text search failed: {str(e)}")


@router.get("/search/hybrid", response_model=SearchResponse)
async def search_hybrid_endpoint(
    query: str = Query(..., description="Search query text"),
    limit: int = Query(5, ge=1, le=100, description="Maximum number of results"),
    vector_weight: float = Query(0.5, ge=0.0, le=1.0, description="Weight for vector scores (0-1)"),
    rerank: bool = Query(True, description="Enable reranking")
):
    """
    Hybrid search combining vector similarity and full-text search.

    - **query**: Search query text
    - **limit**: Maximum number of results (1-100)
    - **vector_weight**: Weight for vector scores (0-1), text weight is (1-vector_weight)
    - **rerank**: Whether to apply two-stage reranking

    Returns list of matching chunks with combined scores.
    """
    try:
        results = hybrid_search(query, limit=limit, vector_weight=vector_weight, rerank=rerank)
        search_results = [SearchResult(**r) for r in results]
        return SearchResponse(results=search_results)
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Hybrid search failed: {str(e)}")


@router.delete("/files", response_model=DeleteResponse)
async def delete_file_endpoint(
    path: str = Query(..., description="Absolute file path to remove")
):
    """
    Remove all chunks for a file from the index.

    - **path**: Absolute file path

    Returns the file path and deletion status.
    """
    try:
        resolved_path, deleted = delete_file(path)
        return DeleteResponse(path=resolved_path, deleted=deleted)
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Deletion failed: {str(e)}")


@router.get("/stats", response_model=StatsResponse)
async def stats_endpoint():
    """
    Get database statistics.

    Returns total chunks, unique files, and a sample chunk.
    """
    try:
        stats = database_stats()
        return StatsResponse(**stats)
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Stats retrieval failed: {str(e)}")


@router.get("/health", response_model=HealthResponse)
async def health_endpoint():
    """
    Health check endpoint.

    Returns server status.
    """
    return HealthResponse(status="ok")


@router.post("/rebuild-fts", response_model=RebuildFtsResponse)
async def rebuild_fts_endpoint():
    """
    Rebuild the FTS5 full-text search index.

    This drops and recreates the FTS5 table, then repopulates it from
    the documents table. Useful when the FTS5 index gets out of sync.

    Returns the number of documents reindexed.
    """
    try:
        count = rebuild_fts_index()
        return RebuildFtsResponse(
            documents_reindexed=count,
            message=f"FTS5 index rebuilt with {count} documents"
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"FTS rebuild failed: {str(e)}")


@router.get("/files", response_model=IndexedFilesResponse)
async def list_files_endpoint():
    """
    List all indexed files.

    Returns a list of all unique file paths in the database.
    """
    try:
        files = get_all_indexed_files()
        return IndexedFilesResponse(files=files, count=len(files))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to list files: {str(e)}")
