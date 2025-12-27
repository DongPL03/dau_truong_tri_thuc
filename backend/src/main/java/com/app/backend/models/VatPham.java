package com.app.backend.models;

import com.app.backend.models.enums.LoaiVatPham;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.*;

/**
 * Entity Vật phẩm (Power-ups/Items) trong trận đấu
 * Các loại vật phẩm:
 * - X2_DIEM: Nhân đôi điểm câu tiếp theo
 * - DONG_BANG_THOI_GIAN: Dừng đồng hồ 5 giây
 * - BO_QUA_CAU_HOI: Bỏ qua câu hỏi hiện tại (không mất điểm)
 * - GOI_Y_50_50: Loại bỏ 2 đáp án sai
 * - KHIEN_BAO_VE: Bảo vệ combo khi trả lời sai
 */
@Entity
@Table(name = "vat_pham")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VatPham {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "ten", nullable = false, length = 100)
    @JsonProperty("ten")
    private String ten;

    @Column(name = "mo_ta", length = 500)
    @JsonProperty("mo_ta")
    private String moTa;

    @Enumerated(EnumType.STRING)
    @Column(name = "loai", nullable = false, length = 50)
    @JsonProperty("loai")
    private LoaiVatPham loai;

    /**
     * Giá trị hiệu ứng (ví dụ: x2 = 2.0, 50-50 loại 2 đáp án = 2)
     */
    @Column(name = "gia_tri_hieu_ung")
    @JsonProperty("gia_tri_hieu_ung")
    @Builder.Default
    private Double giaTriHieuUng = 1.0;

    /**
     * Thời gian hiệu lực (giây) - nếu áp dụng
     */
    @Column(name = "thoi_gian_hieu_luc_giay")
    @JsonProperty("thoi_gian_hieu_luc_giay")
    @Builder.Default
    private Integer thoiGianHieuLucGiay = 0;

    /**
     * Icon hiển thị trên UI
     */
    @Column(name = "icon", length = 50)
    @JsonProperty("icon")
    private String icon;

    /**
     * Màu sắc hiển thị
     */
    @Column(name = "mau_sac", length = 20)
    @JsonProperty("mau_sac")
    private String mauSac;

    /**
     * Giá mua bằng xu trong game (nếu có hệ thống shop)
     */
    @Column(name = "gia_xu")
    @JsonProperty("gia_xu")
    @Builder.Default
    private Integer giaXu = 0;

    /**
     * Có đang active không
     */
    @Column(name = "kich_hoat")
    @JsonProperty("kich_hoat")
    @Builder.Default
    private Boolean kichHoat = true;

    /**
     * Độ hiếm: COMMON, RARE, EPIC, LEGENDARY
     */
    @Column(name = "do_hiem", length = 20)
    @JsonProperty("do_hiem")
    @Builder.Default
    private String doHiem = "COMMON";
}
