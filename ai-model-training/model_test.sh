curl -X POST \
  -H "Authorization: Bearer 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" \
  -H "Content-Type: application/json" \
  -d '{"inputs": "Write a kcats program to produce the first n items of the fibonacci sequence, with n being the top stack item."}' \
  https://api-inference.huggingface.co/models/your-username/your-model-name
