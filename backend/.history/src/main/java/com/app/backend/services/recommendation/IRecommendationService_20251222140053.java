package com.app.backend.services.recommendation;

import com.app.backend.responses.recommendation.RecommendationResponse;

public interface IRecommendationService {

    /**
     * Gợi ý khóa học và bộ câu hỏi nên luyện tập cho user hiện tại.
     *
     * @param userId ID người dùng
     * @return RecommendationResponse chứa danh sách gợi ý
     */
    RecommendationResponse getRecommendationsForUser(Long userId);
}


