package com.app.backend.responses.trandau;

import com.app.backend.models.TranDau;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.time.Instant;

@Data
@Builder
public class TranDauResponse {
    @JsonProperty("id")
    private Long id;

    @JsonProperty("ten_phong")
    private String tenPhong;

    @JsonProperty("ma_phong")
    private String maPhong;

    @JsonProperty("cong_khai")
    private boolean congKhai;

    @JsonProperty("ma_pin")
    private String maPin;

    @JsonProperty("gioi_han_nguoi_choi")
    private int gioiHanNguoiChoi;

    @JsonProperty("gioi_han_thoi_gian_cau_giay")
    private int gioiHanThoiGianCauGiay;

    @JsonProperty("luat_tinh_diem")
    private String luatTinhDiem;

    @JsonProperty("loai_tran_dau")
    private String loaiTranDau;

    @JsonProperty("trang_thai")
    private String trangThai;

    @JsonProperty("chu_phong_ten")
    private String chuPhongTen;

    @JsonProperty("bo_cau_hoi_id")
    private Long boCauHoiId;

    @JsonProperty("bo_cau_hoi_tieu_de")
    private String boCauHoiTieuDe;

    @JsonProperty("tao_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant taoLuc;

    @JsonProperty("bat_dau_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant batDauLuc;

    @JsonProperty("ket_thuc_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ketThucLuc;

    @JsonProperty("so_luong_nguoi_tham_gia")
    private int soLuongNguoiThamGia;

    @JsonProperty("da_tham_gia")
    private boolean daThamGia;   // 👈 NEW

    public static TranDauResponse fromEntity(TranDau td) {
        return TranDauResponse.builder()
                .id(td.getId())
                .tenPhong(td.getTenPhong())
                .maPhong(td.getMaPhong())
                .congKhai(Boolean.TRUE.equals(td.getCongKhai()))
                .maPin(td.getMaPin())
                .gioiHanNguoiChoi(td.getGioiHanNguoiChoi())
                .gioiHanThoiGianCauGiay(td.getGioiHanThoiGianCauGiay())
                .luatTinhDiem(td.getLuatTinhDiem())
                .loaiTranDau(td.getLoaiTranDau())
                .trangThai(td.getTrangThai())
                .chuPhongTen(td.getChuPhong() != null ? td.getChuPhong().getHoTen() : null)
                .boCauHoiId(td.getBoCauHoi() != null ? td.getBoCauHoi().getId() : null)
                .boCauHoiTieuDe(td.getBoCauHoi() != null ? td.getBoCauHoi().getTieuDe() : null)
                .taoLuc(td.getTaoLuc())
                .batDauLuc(td.getBatDauLuc())
                .ketThucLuc(td.getKetThucLuc())
                .build();
    }

    public static TranDauResponse fromEntity(TranDau td, int soLuongNguoiThamGia) {
        return TranDauResponse.builder()
                .id(td.getId())
                .tenPhong(td.getTenPhong())
                .maPhong(td.getMaPhong())
                .congKhai(Boolean.TRUE.equals(td.getCongKhai()))
                .maPin(td.getMaPin())
                .gioiHanNguoiChoi(td.getGioiHanNguoiChoi())
                .gioiHanThoiGianCauGiay(td.getGioiHanThoiGianCauGiay())
                .luatTinhDiem(td.getLuatTinhDiem())
                .loaiTranDau(td.getLoaiTranDau())
                .trangThai(td.getTrangThai())
                .chuPhongTen(td.getChuPhong() != null ? td.getChuPhong().getHoTen() : null)
                .boCauHoiId(td.getBoCauHoi() != null ? td.getBoCauHoi().getId() : null)
                .boCauHoiTieuDe(td.getBoCauHoi() != null ? td.getBoCauHoi().getTieuDe() : null)
                .taoLuc(td.getTaoLuc())
                .batDauLuc(td.getBatDauLuc())
                .ketThucLuc(td.getKetThucLuc())
                .soLuongNguoiThamGia(soLuongNguoiThamGia)
                .build();
    }
}
