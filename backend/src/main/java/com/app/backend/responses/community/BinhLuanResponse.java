package com.app.backend.responses.community;

import com.app.backend.models.BinhLuan;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.time.Instant;
import java.util.List;

@Getter
@Setter
@Builder
public class BinhLuanResponse {

    private Long id;
    private NguoiDangResponse nguoiDang;
    private String noiDung;
    private Integer soLuotThich;
    private Boolean daThich;
    private Boolean biAn;
    private Boolean daSua;
    private Boolean laCuaToi;
    private List<BinhLuanResponse> binhLuanCon;
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

    public static BinhLuanResponse fromEntity(BinhLuan binhLuan, boolean daThich, List<BinhLuanResponse> replies, Long currentUserId, Integer userLevel) {
        boolean laCuaToi = binhLuan.getNguoiBinhLuan().getId().equals(currentUserId);
        boolean daSua = binhLuan.getCapNhatLuc() != null && !binhLuan.getCapNhatLuc().equals(binhLuan.getTaoLuc());

        return BinhLuanResponse.builder()
                .id(binhLuan.getId())
                .nguoiDang(NguoiDangResponse.builder()
                        .id(binhLuan.getNguoiBinhLuan().getId())
                        .ten(binhLuan.getNguoiBinhLuan().getHoTen())
                        .anhDaiDien(binhLuan.getNguoiBinhLuan().getAvatarUrl())
                        .capDo(userLevel != null ? userLevel : 1)
                        .build())
                .noiDung(binhLuan.getNoiDung())
                .soLuotThich(binhLuan.getLuotThich())
                .daThich(daThich)
                .biAn(binhLuan.getBiAn())
                .daSua(daSua)
                .laCuaToi(laCuaToi)
                .binhLuanCon(replies)
                .ngayTao(binhLuan.getTaoLuc())
                .ngayCapNhat(binhLuan.getCapNhatLuc())
                .build();
    }

    public static BinhLuanResponse fromEntitySimple(BinhLuan binhLuan, boolean daThich, Long currentUserId, Integer userLevel) {
        return fromEntity(binhLuan, daThich, null, currentUserId, userLevel);
    }
}
