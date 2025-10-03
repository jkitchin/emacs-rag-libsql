"""Reranker model wrapper using cross-encoder."""

from threading import Lock
from typing import List

from sentence_transformers import CrossEncoder

from ..utils.config import get_settings


class RerankerModel:
    """Thread-safe lazy-loading reranker model wrapper."""

    def __init__(self):
        self._model = None
        self._lock = Lock()

    def _load_model(self) -> CrossEncoder:
        """Load model with thread-safe lazy initialization."""
        if self._model is None:
            with self._lock:
                if self._model is None:
                    settings = get_settings()
                    self._model = CrossEncoder(
                        settings.rerank_model,
                        local_files_only=False
                    )
        return self._model

    def rerank(self, query: str, documents: List[str]) -> List[float]:
        """
        Score query-document pairs.

        Args:
            query: Query text
            documents: List of document texts to score

        Returns:
            List of scores in same order as input documents.
            Higher scores indicate better relevance.
        """
        model = self._load_model()

        # Create query-document pairs
        pairs = [[query, doc] for doc in documents]

        # Score all pairs
        scores = model.predict(
            pairs,
            show_progress_bar=False
        )

        return scores.tolist()


# Global singleton instance
_reranker_model = RerankerModel()


def get_reranker_model() -> RerankerModel:
    """Get the global reranker model instance."""
    return _reranker_model
