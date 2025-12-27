package com.app.backend.responses.khoahoc;

import com.app.backend.models.TienDoKhoaHoc;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.math.BigDecimal;
import java.time.Instant;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TienDoKhoaHoiResponse {

    @JsonProperty("id")
    private Long id;

    @JsonProperty("khoa_hoc_id")
    private Long khoaHocId;

    @JsonProperty("so_bo_da_hoan_thanh")
    private Integer soBoDaHoanThanh;

    @JsonProperty("tong_so_bo")
    private Integer tongSoBo;

    @JsonProperty("phan_tram_hoan_thanh")
    private BigDecimal phanTramHoanThanh;

    @JsonProperty("bo_cau_hoi_hien_tai_id")
    private Long boCauHoiHienTaiId;

    @JsonProperty("trang_thai")
    private String trangThai;

    @JsonProperty("ngay_bat_dau")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ngayBatDau;

    @JsonProperty("ngay_hoan_thanh")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ngayHoanThanh;

    public static TienDoKhoaHoiResponse from(TienDoKhoaHoc entity) {
        return TienDoKhoaHoiResponse.builder()
                .id(entity.getId())
                .khoaHocId(entity.getKhoaHoc() != null ? entity.getKhoaHoc().getId() : null)
                .soBoDaHoanThanh(entity.getSoBoDaHoanThanh())
                .tongSoBo(entity.getTongSoBo())
                .phanTramHoanThanh(entity.getPhanTramHoanThanh())
                .boCauHoiHienTaiId(entity.getBoCauHoiHienTai() != null ? entity.getBoCauHoiHienTai().getId() : null)
                .trangThai(entity.getTrangThai())
                .ngayBatDau(entity.getNgayBatDau())
                .ngayHoanThanh(entity.getNgayHoanThanh())
                .build();
    }
}

