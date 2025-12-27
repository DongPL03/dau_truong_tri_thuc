package com.app.backend.responses.recommendation;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RecommendationResponse {

    @JsonProperty("courses")
    private List<CourseRecommendationItemResponse> courses;

    @JsonProperty("bo_cau_hoi")
    private List<BoCauHoiRecommendationItemResponse> boCauHoi;
}


