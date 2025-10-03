"""Search service with vector similarity and reranking."""

from typing import Any, Dict, List

from ..models.database import query_by_vector
from ..models.embeddings import get_embedding_model
from ..models.reranker import get_reranker_model
from ..utils.config import get_settings


def vector_search(query: str, *, limit: int = 5, rerank: bool = True) -> List[Dict[str, Any]]:
    """
    Semantic search with optional reranking.

    Steps:
    1. Get settings (rerank_enabled, rerank_top_k)
    2. Determine initial_limit based on reranking
    3. Generate query embedding
    4. Query database for top-K results
    5. Format results with metadata
    6. If reranking:
       - Score with cross-encoder
       - Re-sort by scores
       - Truncate to limit
    7. Return formatted results

    Args:
        query: Search query text
        limit: Maximum number of results to return
        rerank: Whether to apply reranking (default: True)

    Returns:
        List of search results with scores
    """
    settings = get_settings()
    embedding_model = get_embedding_model()

    # Determine if we should rerank
    should_rerank = rerank and settings.rerank_enabled

    # Determine initial retrieval limit
    if should_rerank:
        initial_limit = max(settings.rerank_top_k, limit)
    else:
        initial_limit = limit

    # Generate query embedding
    query_embedding = embedding_model.embed_query(query)

    # Query database
    raw_results = query_by_vector(query_embedding, n_results=initial_limit)

    # Format results
    results = _format_results(raw_results)

    # Apply reranking if enabled
    if should_rerank and results:
        results = _rerank_results(query, results, limit)
    else:
        # Just truncate to limit
        results = results[:limit]

    return results


def _format_results(payload: Dict[str, Any]) -> List[Dict[str, Any]]:
    """
    Transform database results to API format.

    Args:
        payload: Raw database results

    Returns:
        List of formatted result dictionaries
    """
    # Extract from nested structure (ChromaDB compatibility format)
    ids = payload['ids'][0] if payload['ids'] else []
    documents = payload['documents'][0] if payload['documents'] else []
    metadatas = payload['metadatas'][0] if payload['metadatas'] else []
    distances = payload['distances'][0] if payload['distances'] else []

    results = []
    for doc, metadata, distance in zip(documents, metadatas, distances):
        # Convert distance to similarity score (1 - distance for cosine)
        # Higher score = better match
        score = 1.0 - distance

        result = {
            'content': doc,
            'source_path': metadata['source_path'],
            'chunk_index': metadata['chunk_index'],
            'line_number': metadata['line_number'],
            'score': score
        }
        results.append(result)

    return results


def _rerank_results(query: str, results: List[Dict[str, Any]], limit: int) -> List[Dict[str, Any]]:
    """
    Rerank results using cross-encoder.

    Steps:
    1. Extract content from results
    2. Create query-document pairs
    3. Score with cross-encoder model
    4. Attach scores to results
    5. Sort by score descending
    6. Return top-N results

    Args:
        query: Search query text
        results: List of initial search results
        limit: Maximum number of results to return

    Returns:
        Reranked and truncated results
    """
    reranker = get_reranker_model()

    # Extract documents
    documents = [result['content'] for result in results]

    # Score with cross-encoder
    scores = reranker.rerank(query, documents)

    # Attach scores to results
    for result, score in zip(results, scores):
        result['score'] = float(score)

    # Sort by score descending (higher is better)
    results.sort(key=lambda x: x['score'], reverse=True)

    # Return top-N
    return results[:limit]
