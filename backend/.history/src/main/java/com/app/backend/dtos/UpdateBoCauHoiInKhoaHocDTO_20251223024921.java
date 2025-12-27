package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class UpdateBoCauHoiInKhoaHocDTO {
    @JsonProperty("thu_tu")
    private Integer thuTu;

    @JsonProperty("is_bat_buoc")
    private Boolean isBatBuoc;

    @JsonProperty("diem_toi_thieu")
    private Integer diemToiThieu;
}

