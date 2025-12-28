package com.app.backend.responses.community;

import com.app.backend.models.BaiViet;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.time.Instant;
import java.util.List;

@Getter
@Setter
@Builder
public class BaiVietResponse {

    private Long id;
    private NguoiDangResponse nguoiDang;
    private String tieuDe;
    private String noiDung;
    private String loai;
    private String trangThai;
    private Integer luotXem;
    private Integer soLuotThich;
    private Integer soLuotBinhLuan;
    private Boolean ghim;
    private List<TagResponse> tags;
    private List<HinhAnhResponse> hinhAnh;
    private Boolean daThich;
    private Boolean daLuu;
    private Boolean laCuaToi;
    private Boolean daSua;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ngayTao;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ngayCapNhat;

    @Getter
    @Setter
    @Builder
    public static class NguoiDangResponse {
        private Long id;
        private String ten;
        private String anhDaiDien;
        private Integer capDo;
    }

    @Getter
    @Setter
    @Builder
    public static class HinhAnhResponse {
        private Long id;
        private String duongDan;
        private Integer thuTu;
    }

    public static BaiVietResponse fromEntity(BaiViet baiViet, boolean daThich, boolean daLuu, Integer userLevel) {
        return fromEntity(baiViet, daThich, daLuu, userLevel, null);
    }

    public static BaiVietResponse fromEntity(BaiViet baiViet, boolean daThich, boolean daLuu, Integer userLevel, Long currentUserId) {
        boolean laCuaToi = baiViet.getNguoiDang().getId().equals(currentUserId);
        boolean daSua = baiViet.getCapNhatLuc() != null && !baiViet.getCapNhatLuc().equals(baiViet.getTaoLuc());

        return BaiVietResponse.builder()
                .id(baiViet.getId())
                .nguoiDang(NguoiDangResponse.builder()
                        .id(baiViet.getNguoiDang().getId())
                        .ten(baiViet.getNguoiDang().getHoTen())
                        .anhDaiDien(baiViet.getNguoiDang().getAvatarUrl())
                        .capDo(userLevel)
                        .build())
                .tieuDe(baiViet.getTieuDe())
                .noiDung(baiViet.getNoiDung())
                .loai(baiViet.getLoaiBai().name())
                .trangThai(baiViet.getTrangThai().name())
                .luotXem(baiViet.getLuotXem())
                .soLuotThich(baiViet.getLuotThich())
                .soLuotBinhLuan(baiViet.getLuotBinhLuan())
                .ghim(baiViet.getGhim())
                .tags(baiViet.getTags().stream()
                        .map(TagResponse::fromEntity)
                        .toList())
                .hinhAnh(baiViet.getHinhAnhs().stream()
                        .<HinhAnhResponse>map(h -> HinhAnhResponse.builder()
                                .id(h.getId())
                                .duongDan(h.getDuongDan())
                                .thuTu(h.getThuTu())
                                .build())
                        .toList())
                .daThich(daThich)
                .daLuu(daLuu)
                .laCuaToi(laCuaToi)
                .daSua(daSua)
                .ngayTao(baiViet.getTaoLuc())
                .ngayCapNhat(baiViet.getCapNhatLuc())
                .build();
    }
}
