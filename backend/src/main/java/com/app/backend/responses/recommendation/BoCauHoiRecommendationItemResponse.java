package com.app.backend.responses.recommendation;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BoCauHoiRecommendationItemResponse {

    @JsonProperty("bo_cau_hoi_id")
    private Long boCauHoiId;

    @JsonProperty("tieu_de")
    private String tieuDe;

    @JsonProperty("chu_de_ten")
    private String chuDeTen;

    @JsonProperty("so_cau_sai")
    private Integer soCauSai;

    @JsonProperty("ly_do")
    private String lyDo;
}


