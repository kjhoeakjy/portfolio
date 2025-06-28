import time
import re
import os
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
from multiprocessing import Process, Manager

# 불용 문구 및 전처리 함수
EXCLUDED_PHRASES = {"매우 아쉬워요", "별로예요", "보통이에요", "마음에 들어요", "최고예요"}

def keep_hangul(text):
    text = re.sub(r'[^가-힣0-9\s]', ' ', text)
    return re.sub(r'\s+', ' ', text).strip()

# ChromeDriver 설정 클래스
class ChromeDriver:
    def __init__(self):
        self.set_options()
        self.set_driver()

    def set_options(self):
        self.options = webdriver.ChromeOptions()
        self.options.add_argument("--headless")
        self.options.add_argument("--disable-gpu")
        self.options.add_argument("--no-sandbox")
        self.options.add_argument("--disable-dev-shm-usage")
        self.options.add_argument("lang=ko_KR")
        self.options.add_argument("--blink-settings=imagesEnabled=false")
        self.options.add_argument("--disable-popup-blocking")
        self.options.add_argument("--window-size=1920,1080")
        self.options.add_argument("user-agent=Mozilla/5.0")
        self.options.add_experimental_option("excludeSwitches", ["enable-automation"])
        self.options.add_experimental_option("useAutomationExtension", False)
        self.options.add_argument("--disable-webgl")

    def set_driver(self):
        self.driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=self.options)

# 전체 리뷰 수집 클래스
class TodayHouse:
    def __init__(self, url, shared_list):
        self.url = url
        self.driver = ChromeDriver().driver
        self.wait = WebDriverWait(self.driver, 5)
        self.shared_list = shared_list
        self.seen = set()

    def collect_reviews(self, max_reviews=50):
        print("[전체] 리뷰 수집 시작")
        collected = 0
        page = 1

        while collected < max_reviews:
            try:
                self.wait.until(EC.presence_of_all_elements_located((By.CSS_SELECTOR, "article.production-review-item")))
                items = self.driver.find_elements(By.CSS_SELECTOR, "article.production-review-item")

                for item in items:
                    if collected >= max_reviews:
                        break
                    try:
                        tag = item.find_element(By.CSS_SELECTOR, "p.production-review-item__description")
                        review_text = tag.text.strip()
                        if review_text in EXCLUDED_PHRASES or review_text in self.seen:
                            continue
                        self.shared_list.append(review_text)
                        self.seen.add(review_text)
                        collected += 1
                    except:
                        continue

                print(f"[전체] 페이지 {page} 완료 → 누적 {collected}/{max_reviews} 수집됨")
                page += 1

                current_page = int(self.driver.find_element(By.CSS_SELECTOR, "button.R16_p").text.strip())
                next_btn = self.driver.find_element(By.XPATH, f"//button[@class='_3b4ci' and text()='{current_page + 1}']")
                self.driver.execute_script("arguments[0].click();", next_btn)
                self.wait.until(EC.staleness_of(items[0]))
            except:
                print(f"[전체] 더 이상 리뷰 없음 (총 {collected}개 수집)")
                break

        return collected

    def run(self):
        try:
            print("[전체] 크롤링 시작")
            self.driver.get(self.url)
            time.sleep(0.5)
            total = self.collect_reviews(max_reviews=50)
            print(f"[전체] 크롤링 완료: {total}개 수집됨")
        except Exception as e:
            print(f"[전체] 에러 발생: {e}")
        finally:
            self.driver.quit()

# multiprocess-safe 실행용 함수
def run_todayhouse(url, shared_list):
    TodayHouse(url, shared_list).run()

# 메인 크롤러 클래스
class Crawler:
    def __init__(self, url):
        self.url = url
        self.product_code = self.extract_product_code()
        self.reviews = []

    def extract_product_code(self):
        match = re.search(r"/productions/(\d+)", self.url)
        if not match:
            raise ValueError("올바른 오늘의집 제품 URL이 아닙니다.")
        return match.group(1)

    def run(self):
        with Manager() as manager:
            shared_list = manager.list()
            p = Process(target=run_todayhouse, args=(self.url, shared_list))
            p.start()
            p.join()

            cleaned_reviews = [keep_hangul(text) for text in list(set(shared_list)) if keep_hangul(text)]
            self.reviews = cleaned_reviews

        return self.reviews, self.product_code
