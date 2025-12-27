package com.app.backend.responses.khoahoc;

import com.app.backend.models.KhoaHocBoCauHoi;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BoCauHoiTrongKhoaResponse {

    @JsonProperty("id")
    private Long id;

    @JsonProperty("bo_cau_hoi_id")
    private Long boCauHoiId;

    @JsonProperty("tieu_de")
    private String tieuDe;

    @JsonProperty("so_cau_hoi")
    private Integer soCauHoi;

    @JsonProperty("thu_tu")
    private Integer thuTu;

    @JsonProperty("is_bat_buoc")
    private Boolean isBatBuoc;

    @JsonProperty("diem_toi_thieu")
    private Integer diemToiThieu;

    @JsonProperty("trang_thai")
    private String trangThai; // CHUA_MO_KHOA, DA_MO_KHOA, DANG_HOC, HOAN_THANH

    @JsonProperty("da_mo_khoa")
    private Boolean daMoKhoa;

    @JsonProperty("can_mo_khoa")
    private Boolean canMoKhoa;

    @JsonProperty("gia_mo_khoa")
    private Long giaMoKhoa;

    @JsonProperty("so_cau_sai")
    private Integer soCauSai; // Số lượng câu hỏi đã trả lời sai (từ the_ghi_nho)

    public static BoCauHoiTrongKhoaResponse from(KhoaHocBoCauHoi entity, String trangThai, Boolean daMoKhoa, Integer soCauSai) {
        return BoCauHoiTrongKhoaResponse.builder()
                .id(entity.getId())
                .boCauHoiId(entity.getBoCauHoi() != null ? entity.getBoCauHoi().getId() : null)
                .tieuDe(entity.getBoCauHoi() != null ? entity.getBoCauHoi().getTieuDe() : null)
                .soCauHoi(entity.getBoCauHoi() != null ? entity.getBoCauHoi().getSoCauHoi() : 0)
                .thuTu(entity.getThuTu())
                .isBatBuoc(entity.getIsBatBuoc())
                .diemToiThieu(entity.getDiemToiThieu())
                .trangThai(trangThai)
                .daMoKhoa(daMoKhoa)
                .canMoKhoa(entity.getBoCauHoi() != null ? entity.getBoCauHoi().getCanMoKhoa() : false)
                .giaMoKhoa(entity.getBoCauHoi() != null ? entity.getBoCauHoi().getGiaMoKhoa() : 0L)
                .soCauSai(soCauSai != null ? soCauSai : 0)
                .build();
    }
}

