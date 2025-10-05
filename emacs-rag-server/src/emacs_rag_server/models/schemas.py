"""Pydantic schemas for request/response validation."""

from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class IndexRequest(BaseModel):
    """Request schema for indexing a file."""
    path: str = Field(..., description="Absolute file path")
    content: Optional[str] = Field(None, description="Content override (optional)")
    metadata: Dict[str, Any] = Field(default_factory=dict, description="Custom metadata")


class IndexResponse(BaseModel):
    """Response schema for indexing operation."""
    path: str
    chunks_indexed: int


class DeleteResponse(BaseModel):
    """Response schema for delete operation."""
    path: str
    deleted: bool


class SearchResult(BaseModel):
    """Single search result."""
    source_path: str
    chunk_index: int
    line_number: int
    content: str
    score: float


class SearchResponse(BaseModel):
    """Response schema for search operations."""
    results: List[SearchResult]


class StatsResponse(BaseModel):
    """Response schema for statistics."""
    total_chunks: int
    total_unique_files: int
    sample_chunk: Optional[Dict[str, Any]] = None


class HealthResponse(BaseModel):
    """Response schema for health check."""
    status: str = "ok"


class RebuildFtsResponse(BaseModel):
    """Response schema for FTS rebuild operation."""
    documents_reindexed: int
    message: str


class IndexedFilesResponse(BaseModel):
    """Response schema for indexed files list."""
    files: List[str]
    count: int


class OrgHeading(BaseModel):
    """Single org heading."""
    source_path: str
    line_number: int
    heading_text: str
    tags: Optional[str] = None
    level: int


class OrgHeadingsResponse(BaseModel):
    """Response schema for org headings list."""
    headings: List[OrgHeading]
    count: int


class OrgHeadingSearchResult(BaseModel):
    """Single org heading search result with similarity score."""
    source_path: str
    line_number: int
    heading_text: str
    tags: Optional[str] = None
    level: int
    score: float


class OrgHeadingSearchResponse(BaseModel):
    """Response schema for org heading semantic search."""
    results: List[OrgHeadingSearchResult]
    count: int
