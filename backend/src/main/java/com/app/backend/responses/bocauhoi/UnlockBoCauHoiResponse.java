package com.app.backend.responses.bocauhoi;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UnlockBoCauHoiResponse {

    @JsonProperty("bo_cau_hoi_id")
    private Long boCauHoiId;

    @JsonProperty("gia_mo_khoa")
    private Long giaMoKhoa;

    @JsonProperty("tien_vang_truoc")
    private Long tienVangTruoc;

    @JsonProperty("tien_vang_sau")
    private Long tienVangSau;

    @JsonProperty("da_mo_khoa_truoc_do")
    private boolean daMoKhoaTruocDo;
}
