package com.app.backend.responses.thongke;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CourseStatResponse {
    @JsonProperty("tong_so_khoa_hoc")
    private Integer tongSoKhoaHoc; // Tổng số khóa học đã tham gia

    @JsonProperty("so_khoa_hoc_da_hoan_thanh")
    private Integer soKhoaHocDaHoanThanh; // Số khóa học đã hoàn thành

    @JsonProperty("so_khoa_hoc_dang_hoc")
    private Integer soKhoaHocDangHoc; // Số khóa học đang học

    @JsonProperty("tong_so_bo_cau_hoi_da_lam")
    private Integer tongSoBoCauHoiDaLam; // Tổng số bộ câu hỏi đã làm (trong các khóa học)

    @JsonProperty("tong_so_bo_cau_hoi_da_hoan_thanh")
    private Integer tongSoBoCauHoiDaHoanThanh; // Tổng số bộ câu hỏi đã hoàn thành

    @JsonProperty("diem_trung_binh")
    private BigDecimal diemTrungBinh; // Điểm trung bình trong tất cả các phiên practice của courses

    @JsonProperty("do_chinh_xac_trung_binh")
    private BigDecimal doChinhXacTrungBinh; // Độ chính xác trung bình (%)

    @JsonProperty("tong_so_phien_luyen_tap")
    private Long tongSoPhienLuyenTap; // Tổng số phiên luyện tập trong courses

    @JsonProperty("tong_thoi_gian_luyen_tap_ms")
    private Long tongThoiGianLuyenTapMs; // Tổng thời gian luyện tập (milliseconds)

    @JsonProperty("phan_tram_hoan_thanh_trung_binh")
    private BigDecimal phanTramHoanThanhTrungBinh; // Phần trăm hoàn thành trung bình của các khóa học
}

