package com.app.backend.responses.khoahoc;

import com.app.backend.models.KhoaHoc;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class KhoaHoiDetailResponse {

    @JsonProperty("khoa_hoc")
    private KhoaHoiResponse khoaHoc;

    @JsonProperty("danh_sach_bo_cau_hoi")
    private List<BoCauHoiTrongKhoaResponse> danhSachBoCauHoi;

    @JsonProperty("tien_do")
    private TienDoKhoaHoiResponse tienDo;

    @JsonProperty("da_mo_khoa_khoa_hoc")
    private Boolean daMoKhoaKhoaHoc;
}

