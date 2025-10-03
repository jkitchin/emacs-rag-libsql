"""Statistics service for database metrics."""

from typing import Any, Dict

from ..models.database import (
    get_sample_chunk,
    get_total_chunks,
    get_total_unique_files,
)


def database_stats() -> Dict[str, Any]:
    """
    Get index statistics.

    Returns:
        Dictionary with:
        - total_chunks: Total number of chunks
        - total_unique_files: Number of unique files
        - sample_chunk: Sample chunk data (or None if empty)
    """
    return {
        'total_chunks': get_total_chunks(),
        'total_unique_files': get_total_unique_files(),
        'sample_chunk': get_sample_chunk()
    }
