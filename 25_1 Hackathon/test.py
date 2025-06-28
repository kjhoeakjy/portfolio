from metric import accuracy, Logger
import torch.nn as nn

class Tester(nn.Module):
    def __init__(self, model, dataloader):
        super().__init__()
        self.model = model
        self.dataloader = dataloader
        self.logger = Logger()

    def test(self):
        self.logger.init()
        for batch, label in self.dataloader:
            predict = self.model(batch)
            metric = accuracy(predict, label)
            self.logger(metric)

    def log(self):
        self.logger.summary()
