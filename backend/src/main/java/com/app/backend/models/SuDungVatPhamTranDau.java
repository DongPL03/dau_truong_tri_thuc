package com.app.backend.models;

import com.app.backend.models.enums.LoaiVatPham;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

/**
 * Entity ghi lại lịch sử sử dụng vật phẩm trong trận đấu
 * Dùng để tracking và phân tích
 */
@Entity
@Table(name = "su_dung_vat_pham_tran_dau")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SuDungVatPhamTranDau {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "tran_dau_id", nullable = false)
    @JsonProperty("tran_dau")
    private TranDau tranDau;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    @JsonProperty("nguoi_dung")
    private NguoiDung nguoiDung;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "vat_pham_id", nullable = false)
    @JsonProperty("vat_pham")
    private VatPham vatPham;

    /**
     * Loại vật phẩm (denormalized để query nhanh)
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "loai_vat_pham", nullable = false, length = 50)
    @JsonProperty("loai_vat_pham")
    private LoaiVatPham loaiVatPham;

    /**
     * Index câu hỏi khi sử dụng vật phẩm
     */
    @Column(name = "cau_hoi_index")
    @JsonProperty("cau_hoi_index")
    private Integer cauHoiIndex;

    /**
     * Thời điểm sử dụng
     */
    @Column(name = "su_dung_luc", nullable = false)
    @JsonProperty("su_dung_luc")
    private LocalDateTime suDungLuc;

    /**
     * Kết quả (điểm bonus đã nhận, đáp án đã loại, etc.)
     */
    @Column(name = "ket_qua", length = 500)
    @JsonProperty("ket_qua")
    private String ketQua;
}
