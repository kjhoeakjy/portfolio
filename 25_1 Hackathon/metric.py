# metric.py
def accuracy(pred, target):
    pred_label = pred.argmax(dim=1)
    correct = (pred_label == target).float().sum().item()
    total = target.size(0)
    return correct / total

class Logger:
    def __init__(self):
        self.metrics = []

    def init(self):
        self.metrics = []

    def __call__(self, value):
        self.metrics.append(value)

    def summary(self):
        if self.metrics:
            mean_acc = sum(self.metrics) / len(self.metrics)
            print(f"Test Accuracy: {mean_acc:.4f}")
        else:
            print("No metrics recorded.")