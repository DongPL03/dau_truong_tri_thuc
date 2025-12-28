package com.app.backend.responses.community;

import com.app.backend.models.Tag;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class TagResponse {

    private Long id;
    private String ten;
    private String slug;
    private String moTa;
    private String mauSac;
    private String icon;
    private Integer soBaiViet;
    private Integer thuTu;
    private Boolean hienThi;

    public static TagResponse fromEntity(Tag tag) {
        return TagResponse.builder()
                .id(tag.getId())
                .ten(tag.getTen())
                .slug(tag.getSlug())
                .moTa(tag.getMoTa())
                .mauSac(tag.getMauSac())
                .icon(tag.getIcon())
                .soBaiViet(tag.getSoBaiViet())
                .thuTu(tag.getThuTu())
                .hienThi(tag.getHienThi())
                .build();
    }
}
