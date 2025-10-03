"""Embedding model wrapper using sentence-transformers."""

from threading import Lock
from typing import List

from sentence_transformers import SentenceTransformer

from ..utils.config import get_settings


class EmbeddingModel:
    """Thread-safe lazy-loading embedding model wrapper."""

    def __init__(self):
        self._model = None
        self._lock = Lock()

    def _load_model(self) -> SentenceTransformer:
        """Load model with thread-safe lazy initialization."""
        if self._model is None:
            with self._lock:
                if self._model is None:
                    settings = get_settings()
                    self._model = SentenceTransformer(
                        settings.embedding_model,
                        local_files_only=False
                    )
        return self._model

    def embed_documents(self, documents: List[str]) -> List[List[float]]:
        """
        Batch encode documents with normalization.

        Args:
            documents: List of text documents to embed

        Returns:
            List of embedding vectors (normalized)
        """
        model = self._load_model()
        embeddings = model.encode(
            documents,
            convert_to_numpy=True,
            show_progress_bar=False,
            normalize_embeddings=True
        )
        return embeddings.tolist()

    def embed_query(self, query: str) -> List[float]:
        """
        Encode single query with normalization.

        Args:
            query: Query text to embed

        Returns:
            Embedding vector (normalized)
        """
        model = self._load_model()
        embedding = model.encode(
            [query],
            convert_to_numpy=True,
            show_progress_bar=False,
            normalize_embeddings=True
        )
        return embedding[0].tolist()


# Global singleton instance
_embedding_model = EmbeddingModel()


def get_embedding_model() -> EmbeddingModel:
    """Get the global embedding model instance."""
    return _embedding_model
