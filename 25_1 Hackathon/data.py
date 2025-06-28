from torchvision import datasets, transforms
from torch.utils.data import DataLoader as TorchLoader
from PIL import Image
from torchvision.transforms.autoaugment import TrivialAugmentWide
import torch
import os

class DataLoader:
    def __init__(self, root, batch_size=64, image_size=512, shuffle=True, use_autoaugment=True):
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

        # 임의 샘플 이미지 하나 열어보기
        sample_image_path, _ = datasets.ImageFolder(root=root).samples[0]
        with Image.open(sample_image_path) as img:
            width, height = img.size

        # transform 구성
        aug_transforms = [
            transforms.Lambda(lambda img: img.convert("RGB")),
            transforms.Resize((image_size, image_size))
        ]

        if use_autoaugment:
            aug_transforms += [
                transforms.RandomHorizontalFlip(),
                TrivialAugmentWide()
            ]

        aug_transforms += [
            transforms.ToTensor(),
            transforms.Normalize(
                mean=(0.485, 0.456, 0.406),
                std=(0.229, 0.224, 0.225)
            )
        ]

        transform = transforms.Compose(aug_transforms)

        dataset = datasets.ImageFolder(root=root, transform=transform)
        self.dataset = dataset
        self.loader = TorchLoader(dataset, batch_size=batch_size, shuffle=shuffle, num_workers=4, pin_memory=True)
        self.num_classes = len(dataset.classes)
        self.input_channels = 3

    def __iter__(self):
        for x, y in self.loader:
            yield x.to(self.device), y.to(self.device)

    def __len__(self):
        return len(self.loader)

    def get_shape(self):
        return self.input_channels
