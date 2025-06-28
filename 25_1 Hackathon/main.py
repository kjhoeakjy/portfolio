# main.py
from model import Model
from train import Trainer
from data import DataLoader
from test import Tester

train_root = "/home/team2/datasets/poseidon/train"
test_root = "/home/team2/datasets/poseidon/test"

# 데이터 로딩
train_loader = DataLoader(train_root)
num_classes = train_loader.num_classes

# 모델 정의 및 학습
trainer = Trainer(Model(), train_loader)
trainer.fit()
trainer.log()

# 테스트 실행 (test 단계 전용)
test_loader = DataLoader(test_root, shuffle=False)
tester = Tester(trainer.model, test_loader)
tester.test()
tester.log()
