import torch
import torch.nn as nn
import torch.optim as optim
from torch.optim.lr_scheduler import CosineAnnealingLR
import time
from torch.utils.data import random_split, DataLoader as TorchLoader, Subset
from collections import defaultdict
import random

def stratified_split(dataset, test_size=0.2, seed=42):
    if not hasattr(dataset, 'targets'):
        raise AttributeError("dataset must have a 'targets' attribute for stratified split.")

    label_to_indices = defaultdict(list)
    for idx, label in enumerate(dataset.targets):
        label_to_indices[label].append(idx)

    train_indices = []
    val_indices = []
    random.seed(seed)

    for label, indices in label_to_indices.items():
        random.shuffle(indices)
        n_val = int(len(indices) * test_size)
        val_indices.extend(indices[:n_val])
        train_indices.extend(indices[n_val:])

    random.shuffle(train_indices)
    random.shuffle(val_indices)

    return Subset(dataset, train_indices), Subset(dataset, val_indices)

def accuracy(pred, target):
    pred_label = pred.argmax(dim=1)
    correct = (pred_label == target).float().sum().item()
    total = target.size(0)
    return correct / total

class WarmupScheduler:
    def __init__(self, optimizer, warmup_epochs, base_scheduler):
        self.optimizer = optimizer
        self.warmup_epochs = warmup_epochs
        self.base_scheduler = base_scheduler
        self.current_epoch = 0

    def step(self, metric=None):
        self.current_epoch += 1
        if self.current_epoch <= self.warmup_epochs:
            for group in self.optimizer.param_groups:
                group['lr'] = group['initial_lr'] * self.current_epoch / self.warmup_epochs
        else:
            self.base_scheduler.step()

class Trainer:
    def __init__(self, model, dataloader, val_dataloader=None, device='cuda'):
        self.device = device
        self.model = model.to(self.device)
        self.criterion = model.loss_fn
        self.optimizer = optim.SGD(self.model.parameters(), lr=0.01, momentum=0.9)
        for group in self.optimizer.param_groups:
            group['initial_lr'] = group['lr']
        base_scheduler = CosineAnnealingLR(self.optimizer, T_max=30)
        self.scheduler = WarmupScheduler(self.optimizer, warmup_epochs=2, base_scheduler=base_scheduler)
        self.logs = []

        # If val_dataloader is not provided, split the dataloader.dataset
        if val_dataloader is None and hasattr(dataloader, 'dataset'):
            dataset = dataloader.dataset
            if hasattr(dataset, 'targets'):
                train_dataset, val_dataset = stratified_split(dataset, test_size=0.1)
            else:
                val_size = int(0.1 * len(dataset))
                train_size = len(dataset) - val_size
                train_dataset, val_dataset = random_split(dataset, [train_size, val_size])

            loader_config = {
                "batch_size": dataloader.loader.batch_size,
                "num_workers": dataloader.loader.num_workers,
                "pin_memory": dataloader.loader.pin_memory
            }

            self.dataloader = TorchLoader(train_dataset, shuffle=True, **loader_config)
            self.val_dataloader = TorchLoader(val_dataset, shuffle=False, **loader_config)
        else:
            self.dataloader = dataloader
            self.val_dataloader = val_dataloader

    def evaluate(self):
        self.model.eval()
        total_loss = 0.0
        correct = 0
        total = 0
        with torch.no_grad():
            for batch, label in self.val_dataloader:
                batch = batch.to(self.device)
                label = label.to(self.device)
                pred = self.model(batch)
                loss = self.criterion(pred, label)
                total_loss += loss.item()
                correct += (pred.argmax(dim=1) == label).sum().item()
                total += label.size(0)
        return total_loss / len(self.val_dataloader), correct / total

    def fit(self, max_epoch=60, max_minutes=55):
        best_val_loss = float('inf')
        no_improve_count = 0
        start_time = time.time()

        for epoch in range(max_epoch):
            self.model.train()
            running_loss = 0.0
            correct = 0
            total = 0

            for batch, label in self.dataloader:
                batch, label = batch.to(self.device), label.to(self.device)
                pred = self.model(batch)
                loss = self.criterion(pred, label)

                self.optimizer.zero_grad()
                loss.backward()
                self.optimizer.step()

                running_loss += loss.item()
                correct += (pred.argmax(dim=1) == label).sum().item()
                total += label.size(0)

            avg_loss = running_loss / len(self.dataloader)
            acc = correct / total

            if self.val_dataloader is not None:
                val_loss, val_acc = self.evaluate()
                log_msg = (f"[Epoch {epoch+1:2}] Train Loss: {avg_loss:.4f} | "
                           f"Train Acc: {acc:.4f} | Val Loss: {val_loss:.4f} | Val Acc: {val_acc:.4f}")
            else:
                log_msg = f"[Epoch {epoch+1:2}] Train Loss: {avg_loss:.4f} | Train Acc: {acc:.4f}"

            self.scheduler.step(acc)
            print(log_msg)
            self.logs.append(log_msg)

            if self.val_dataloader is not None:
                if val_loss < best_val_loss:
                    best_val_loss = val_loss
                    no_improve_count = 0
                else:
                    no_improve_count += 1
                    if no_improve_count >= 8:
                        print("Validation loss has not improved for 8 epochs. Stopping training.")
                        break

            elapsed_minutes = (time.time() - start_time) / 60
            if elapsed_minutes >= max_minutes:
                print(f"Training time exceeded {max_minutes} minutes. Stopping training.")
                break

    def log(self):
        print("Training finished.")
        with open(f"/home/team2/logs/{time.time()}.txt", "w", encoding="utf-8") as f:
            for log in self.logs:
                f.write(log + "\n")
