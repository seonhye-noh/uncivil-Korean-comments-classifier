import pandas as pd
import re

def clean_text(series: pd.Series) -> pd.Series:
    return (
        series.astype("string")
              .str.replace(r"\s+", " ", regex=True)
              .str.strip()
    )

def recode_label(df: pd.DataFrame) -> pd.DataFrame:
    """
    hiddenByCleanbot:
    1 → flagged (uncivil)
    0 → not flagged
    """
    df["hiddenByCleanbot"] = df["hiddenByCleanbot"].map({True: 1, False: 0})
    return df