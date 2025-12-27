package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
public class AddBoCauHoiToKhoaHocDTO {
    @NotNull
    @JsonProperty("bo_cau_hoi_id")
    private Long boCauHoiId;

    @NotNull
    @JsonProperty("thu_tu")
    private Integer thuTu;

    @JsonProperty("is_bat_buoc")
    private Boolean isBatBuoc = true;

    @JsonProperty("diem_toi_thieu")
    private Integer diemToiThieu = 0;
}

