package com.app.backend.responses.community;

import com.app.backend.models.BaoCao;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.time.Instant;

@Getter
@Setter
@Builder
public class BaoCaoResponse {

    private Long id;
    private BaiVietInfo baiViet;
    private BinhLuanInfo binhLuan;
    private NguoiBaoCaoResponse nguoiBaoCao;
    private String loai;
    private String chiTiet;
    private String trangThai;
    private NguoiBaoCaoResponse nguoiXuLy;
    private String ghiChuXuLy;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ngayTao;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ngayXuLy;

    @Getter
    @Setter
    @Builder
    public static class BaiVietInfo {
        private Long id;
        private String tieuDe;
    }

    @Getter
    @Setter
    @Builder
    public static class BinhLuanInfo {
        private Long id;
        private String noiDung;
    }

    @Getter
    @Setter
    @Builder
    public static class NguoiBaoCaoResponse {
        private Long id;
        private String ten;
        private String anhDaiDien;
    }

    public static BaoCaoResponse fromEntity(BaoCao baoCao) {
        BaoCaoResponseBuilder builder = BaoCaoResponse.builder()
                .id(baoCao.getId())
                .nguoiBaoCao(NguoiBaoCaoResponse.builder()
                        .id(baoCao.getNguoiBaoCao().getId())
                        .ten(baoCao.getNguoiBaoCao().getHoTen())
                        .anhDaiDien(baoCao.getNguoiBaoCao().getAvatarUrl())
                        .build())
                .loai(baoCao.getLoaiBaoCao().name())
                .chiTiet(baoCao.getChiTiet())
                .trangThai(baoCao.getTrangThai().name())
                .ngayTao(baoCao.getTaoLuc());

        if (baoCao.getBaiViet() != null) {
            builder.baiViet(BaiVietInfo.builder()
                    .id(baoCao.getBaiViet().getId())
                    .tieuDe(baoCao.getBaiViet().getTieuDe())
                    .build());
        }

        if (baoCao.getBinhLuan() != null) {
            String noiDung = baoCao.getBinhLuan().getNoiDung();
            if (noiDung.length() > 100) {
                noiDung = noiDung.substring(0, 100) + "...";
            }
            builder.binhLuan(BinhLuanInfo.builder()
                    .id(baoCao.getBinhLuan().getId())
                    .noiDung(noiDung)
                    .build());
        }

        if (baoCao.getXuLyBoi() != null) {
            builder.nguoiXuLy(NguoiBaoCaoResponse.builder()
                    .id(baoCao.getXuLyBoi().getId())
                    .ten(baoCao.getXuLyBoi().getHoTen())
                    .anhDaiDien(baoCao.getXuLyBoi().getAvatarUrl())
                    .build());
            builder.ngayXuLy(baoCao.getXuLyLuc());
            builder.ghiChuXuLy(baoCao.getGhiChuXuLy());
        }

        return builder.build();
    }
}
