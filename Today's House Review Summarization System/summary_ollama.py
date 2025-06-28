import requests
import os
import json
import re

class Ollama:
    def __init__(self, reviews, save_dir, product_code):
        self.reviews = reviews
        self.save_dir = save_dir
        self.product_code = product_code
        self.model = "phi"  # 모델명 (예: gemma:2b-instruct, openchat 등)
        os.makedirs(self.save_dir, exist_ok=True)

    def parse_stream_response(self, response):
        contents = []
        for line in response.iter_lines():
            if line:
                try:
                    data = json.loads(line.decode('utf-8'))
                    if "message" in data and "content" in data["message"]:
                        contents.append(data["message"]["content"])
                except Exception as e:
                    print(f"[ERROR] JSON 파싱 실패: {e}")
                    continue
        full_text = "".join(contents).strip()
        if not full_text:
            print("[WARN] 요약 결과가 비어 있습니다.")
        return full_text

    def summarize_all(self):
        if not self.reviews:
            return "[장점 요약]\n리뷰가 부족합니다.\n\n[단점 요약]\n리뷰가 부족합니다."

        review_text = "\n".join(self.reviews[:50])
        prompt = f"""
다음은 한 제품에 대한 사용자 리뷰 모음입니다.  
이 리뷰들을 바탕으로 제품의 '장점'과 '단점'을 구분하여 요약해 주세요.

- 반드시 한국어로 작성해 주세요.
- [장점 요약], [단점 요약] 형식으로 작성해 주세요.
- 중복 표현 없이 간결하고 구체적인 문장으로 요약해 주세요.

[리뷰 모음]
{review_text}
"""

        try:
            response = requests.post(
                "http://localhost:11434/api/chat",
                json={
                    "model": self.model,
                    "messages": [{"role": "user", "content": prompt}],
                    "stream": True
                },
                stream=True,
                timeout=180
            )

            if response.status_code != 200:
                print(f"[ERROR] Ollama 응답 오류: {response.status_code} {response.text}")
                return "[장점 요약]\n요약 실패\n\n[단점 요약]\n요약 실패"

            result = self.parse_stream_response(response)
            return result if result else "[장점 요약]\n요약 없음\n\n[단점 요약]\n요약 없음"

        except Exception as e:
            print(f"[ERROR] 요약 요청 실패: {e}")
            return "[장점 요약]\n요약 실패\n\n[단점 요약]\n요약 실패"

    def run(self):
        print("[INFO] 전체 리뷰에서 장점과 단점 요약 중...")
        summary = self.summarize_all()

        safe_code = self.product_code if self.product_code.isdigit() else "unknown"
        save_path = os.path.join(self.save_dir, f"{safe_code}_summary.txt")
        with open(save_path, "w", encoding="utf-8") as f:
            f.write(summary)

        print(f"[INFO] 요약 저장 완료: {save_path}")

        # 출력용 추출
        def extract_block(text, keyword):
            match = re.search(rf"\[{keyword} 요약\](.*?)(?:\n\[|$)", text, re.DOTALL)
            return match.group(1).strip() if match else f"{keyword} 요약을 찾을 수 없습니다."

        summary_pos = extract_block(summary, "장점")
        summary_neg = extract_block(summary, "단점")

        print("[요약 예시 출력]")
        print(">>> [장점 요약] ", summary_pos[:100].replace('\n', ' ') + "...")
        print(">>> [단점 요약] ", summary_neg[:100].replace('\n', ' ') + "...")
