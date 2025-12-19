package com.app.backend.responses.lichsutrandau;

import com.app.backend.models.LichSuTranDau;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.Instant;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LichSuTranDauResponse {

    @JsonProperty("lich_su_id")
    private Long lichSuId;

    @JsonProperty("tran_dau_id")
    private Long tranDauId;

    @JsonProperty("ten_phong")
    private String tenPhong;

    @JsonProperty("bo_cau_hoi_tieu_de")
    private String boCauHoiTieuDe;

    @JsonProperty("ten_hien_thi")
    private String tenHienThi;

    @JsonProperty("tong_diem")
    private Integer tongDiem;

    @JsonProperty("so_cau_dung")
    private Integer soCauDung;

    @JsonProperty("tong_thoi_gian_ms")
    private Long tongThoiGianMs;

    @JsonProperty("xep_hang")
    private Integer xepHang;

    @JsonProperty("hoan_thanh_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant hoanThanhLuc;

    @JsonProperty("luat_tinh_diem")
    private String luatTinhDiem;

    @JsonProperty("loai_tran_dau")
    private String loaiTranDau;


    public static LichSuTranDauResponse fromEntity(LichSuTranDau e) {
        return LichSuTranDauResponse.builder()
                .lichSuId(e.getId())
                .tranDauId(e.getTranDau().getId())
                .tenPhong(e.getTranDau().getTenPhong())
                .boCauHoiTieuDe(
                        e.getTranDau().getBoCauHoi() != null
                                ? e.getTranDau().getBoCauHoi().getTieuDe()
                                : null
                )
                .tenHienThi(e.getNguoiDung().getTenHienThi())
                .tongDiem(e.getTongDiem())
                .soCauDung(e.getSoCauDung())
                .tongThoiGianMs(Long.valueOf(e.getTongThoiGianMs()))
                .xepHang(e.getXepHang())
                .hoanThanhLuc(e.getHoanThanhLuc())
                .luatTinhDiem(e.getTranDau().getLuatTinhDiem())
                .loaiTranDau(e.getTranDau().getLoaiTranDau())
                .build();
    }
}
