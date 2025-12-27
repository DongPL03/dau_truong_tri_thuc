package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

/**
 * Entity kho vật phẩm của người dùng (Inventory)
 * Lưu trữ số lượng vật phẩm mỗi user đang sở hữu
 */
@Entity
@Table(name = "vat_pham_nguoi_dung",
        uniqueConstraints = @UniqueConstraint(columnNames = {"nguoi_dung_id", "vat_pham_id"}))
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VatPhamNguoiDung {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    @JsonProperty("nguoi_dung")
    private NguoiDung nguoiDung;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "vat_pham_id", nullable = false)
    @JsonProperty("vat_pham")
    private VatPham vatPham;

    /**
     * Số lượng vật phẩm đang có
     */
    @Column(name = "so_luong", nullable = false)
    @JsonProperty("so_luong")
    @Builder.Default
    private Integer soLuong = 0;

    /**
     * Thời điểm nhận vật phẩm gần nhất
     */
    @Column(name = "nhan_luc")
    @JsonProperty("nhan_luc")
    private LocalDateTime nhanLuc;

    /**
     * Thời điểm sử dụng gần nhất
     */
    @Column(name = "su_dung_luc")
    @JsonProperty("su_dung_luc")
    private LocalDateTime suDungLuc;
}
