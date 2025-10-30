package com.app.backend.responses.trandau;

import com.app.backend.models.TranDau;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class TranDauResponse {
    private Long id;
    private String tenPhong;
    private String maPhong;
    private boolean congKhai;
    private String maPin;
    private int gioiHanNguoiChoi;
    private int gioiHanThoiGianCauGiay;
    private String luatTinhDiem;
    private String trangThai;
    private String chuPhongTen;
    private Long boCauHoiId;
    private String boCauHoiTieuDe;
    private LocalDateTime taoLuc;
    private LocalDateTime batDauLuc;
    private LocalDateTime ketThucLuc;

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
                .trangThai(td.getTrangThai())
                .chuPhongTen(td.getChuPhong() != null ? td.getChuPhong().getHoTen() : null)
                .boCauHoiId(td.getBoCauHoi() != null ? td.getBoCauHoi().getId() : null)
                .boCauHoiTieuDe(td.getBoCauHoi() != null ? td.getBoCauHoi().getTieuDe() : null)
                .taoLuc(td.getTaoLuc())
                .batDauLuc(td.getBatDauLuc())
                .ketThucLuc(td.getKetThucLuc())
                .build();
    }
}
