import pandas as pd
import numpy as np
from transformers import Trainer, TrainingArguments
from sklearn.metrics import accuracy_score

from utils import set_seed, get_device
from preprocessing import clean_text, recode_label
from dataset import SingleSentDataset
from model import load_model, load_tokenizer
from evaluation import compute_metrics, plot_confusion

# ======================
# Setup
# ======================
set_seed()
device = get_device()

# ======================
# Load data
# ======================
train = pd.read_feather("Cleanbot2.0_train.feather")
val   = pd.read_feather("Cleanbot2.0_validation.feather")

train = recode_label(train)
val   = recode_label(val)

train["contents"] = clean_text(train["contents"])
val["contents"]   = clean_text(val["contents"])

# ======================
# Tokenization
# ======================
tokenizer = load_tokenizer()

train_enc = tokenizer(
    train["contents"].tolist(),
    truncation=True,
    padding=True
)
val_enc = tokenizer(
    val["contents"].tolist(),
    truncation=True,
    padding=True
)

train_ds = SingleSentDataset(train_enc, train["hiddenByCleanbot"].values)
val_ds   = SingleSentDataset(val_enc, val["hiddenByCleanbot"].values)

# ======================
# Model & Trainer
# ======================
model = load_model()

training_args = TrainingArguments(
    output_dir="./results_cleanbot2",
    evaluation_strategy="epoch",
    save_strategy="epoch",
    learning_rate=2e-5,
    per_device_train_batch_size=16,
    per_device_eval_batch_size=16,
    num_train_epochs=3,
    weight_decay=0.01,
    logging_steps=100,
    load_best_model_at_end=True,
    metric_for_best_model="accuracy",
    report_to="none"
)

def compute_metrics_trainer(eval_pred):
    logits, labels = eval_pred
    preds = np.argmax(logits, axis=1)
    return {"accuracy": accuracy_score(labels, preds)}

trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=train_ds,
    eval_dataset=val_ds,
    tokenizer=tokenizer,
    compute_metrics=compute_metrics_trainer
)

trainer.train()
trainer.save_model("./results_cleanbot2")

# ======================
# Evaluation (confusion matrix)
# ======================
preds = trainer.predict(val_ds)
y_true = val["hiddenByCleanbot"].values
y_pred = np.argmax(preds.predictions, axis=1)

metrics = compute_metrics(y_true, y_pred)
pd.DataFrame([metrics]).to_csv(
    "./results_cleanbot2/metrics.csv", index=False
)

plot_confusion(
    y_true,
    y_pred,
    "./results_cleanbot2/confusion_matrix.png"
)
