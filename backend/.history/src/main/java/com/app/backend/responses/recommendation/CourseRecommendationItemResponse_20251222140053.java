package com.app.backend.responses.recommendation;

import com.app.backend.responses.khoahoc.KhoaHoiResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CourseRecommendationItemResponse {

    @JsonProperty("khoa_hoc")
    private KhoaHoiResponse khoaHoc;

    @JsonProperty("ly_do")
    private String lyDo;
}


