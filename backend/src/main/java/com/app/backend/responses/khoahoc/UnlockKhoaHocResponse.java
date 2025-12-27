package com.app.backend.responses.khoahoc;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UnlockKhoaHocResponse {

    @JsonProperty("khoa_hoc_id")
    private Long khoaHocId;

    @JsonProperty("gia_mo_khoa")
    private Long giaMoKhoa;

    @JsonProperty("tien_vang_truoc")
    private Long tienVangTruoc;

    @JsonProperty("tien_vang_sau")
    private Long tienVangSau;

    @JsonProperty("da_mo_khoa_truoc_do")
    private boolean daMoKhoaTruocDo;
}

