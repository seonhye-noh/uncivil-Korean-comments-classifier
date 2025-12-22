from transformers import (
    AutoTokenizer,
    AutoModelForSequenceClassification
)

MODEL_NAME = "beomi/KcELECTRA-base-v2022"

def load_tokenizer():
    return AutoTokenizer.from_pretrained(MODEL_NAME)

def load_model(num_labels=2):
    return AutoModelForSequenceClassification.from_pretrained(
        MODEL_NAME,
        num_labels=num_labels
    )