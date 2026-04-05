from dataclasses import asdict, dataclass
from typing import Any


@dataclass(frozen=True)
class DeepSurvConsensusConfig:
    lr: float
    weight_decay: float
    batch_size: int
    dropout: float
    nodes: tuple[int, ...]
    selection_rule: str
    rationale: tuple[str, ...]


CONSENSUS_CONFIG = DeepSurvConsensusConfig(
    lr=0.0008,
    weight_decay=0.00025,
    batch_size=256,
    dropout=0.52,
    nodes=(256, 256, 128),
    selection_rule="consistency_over_best_single_trial",
    rationale=(
        "Learning rate 0.0008 stays inside the same stable 1e-3 neighborhood seen across the top runs.",
        "Weight decay 0.00025 is retained as the converged value from the earlier notebook.",
        "Batch size 256 is treated as the stable mode; 1024 looked like an outlier.",
        "Dropout 0.52 keeps the model near the strongest targeted trials without overfitting to one winner.",
        "Architecture [256, 256, 128] is the most frequently validated configuration across phases.",
    ),
)

BEST_LR = CONSENSUS_CONFIG.lr
BEST_WD = CONSENSUS_CONFIG.weight_decay
BEST_BATCH = CONSENSUS_CONFIG.batch_size
BEST_DROPOUT = CONSENSUS_CONFIG.dropout
BEST_NODES = list(CONSENSUS_CONFIG.nodes)


def get_deepsurv_consensus_config() -> dict[str, Any]:
    """Return the final DeepSurv configuration as notebook-friendly primitives."""
    config = asdict(CONSENSUS_CONFIG)
    config["nodes"] = list(CONSENSUS_CONFIG.nodes)
    return config


def get_notebook_assignment_block() -> str:
    """Return a pasteable constant block for notebooks that should not be modified in place."""
    return "\n".join(
        [
            f"BEST_LR = {BEST_LR}",
            f"BEST_WD = {BEST_WD}",
            f"BEST_BATCH = {BEST_BATCH}",
            f"BEST_DROPOUT = {BEST_DROPOUT}",
            f"BEST_NODES = {BEST_NODES}",
        ]
    )


if __name__ == "__main__":
    print("Final DeepSurv consensus configuration")
    print(get_notebook_assignment_block())
    print("\nSelection rule:", CONSENSUS_CONFIG.selection_rule)
    print("Rationale:")
    for item in CONSENSUS_CONFIG.rationale:
        print(f"- {item}")
