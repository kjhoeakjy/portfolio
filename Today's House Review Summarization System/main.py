# main.py
from crawler import Crawler
from summary_ollama import Ollama
import time

if __name__ == "__main__":
    url = input("오늘의집 제품 URL을 입력하세요: ").strip()
    start = time.time()

    crawler = Crawler(url)
    rating_dict, product_code = crawler.run()

    summary = Ollama(rating_dict, save_dir=r"C:/Users/kjhoe/OneDrive/바탕화~1-LAPTOP-VGOF9FJ7-25904429/KUBIG/25-1 conference", product_code=product_code)
    summary.run()

    print(f"[INFO] 전체 소요 시간: {time.time() - start:.2f}초")
