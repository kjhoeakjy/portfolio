# model.py
import torch
import torch.nn as nn
import torchvision.models as models

class Model(nn.Module):
    def __init__(self):
        super().__init__()
        net = models.efficientnet_b2(weights=models.EfficientNet_B2_Weights.IMAGENET1K_V1)
        self.backbone = net.features
        self.pool = nn.AdaptiveAvgPool2d((1, 1))
        self.dropout = nn.Dropout(p=0.3)  
        self.in_features = net.classifier[1].in_features
        self.classifier = nn.Identity()  # 나중에 configure 시 설정됨
        self.loss_fn = nn.CrossEntropyLoss(label_smoothing=0.1)
        self.num_classes_set = False

    def forward(self, x):
        x = self.backbone(x)
        x = self.pool(x)
        x = torch.flatten(x, 1)
        x = self.dropout(x)  # Dropout 적용
        x = self.classifier(x)
        return x

    def compute_loss(self, pred, target):
        return self.loss_fn(pred, target)

    def set_output_classes(self, num_classes):
        self.classifier = nn.Linear(self.in_features, num_classes)
        self.num_classes_set = True

    def configure(self, dataloader):
        if not self.num_classes_set:
            self.set_output_classes(dataloader.num_classes)
