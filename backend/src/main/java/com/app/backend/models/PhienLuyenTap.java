package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Entity
@Table(name = "phien_luyen_tap")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PhienLuyenTap {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    private NguoiDung nguoiDung;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "bo_cau_hoi_id", nullable = false)
    private BoCauHoi boCauHoi;

    @Column(name = "tong_cau_hoi", nullable = false)
    private Integer tongCauHoi;

    @Column(name = "so_cau_dung", nullable = false)
    private Integer soCauDung;

    @Column(name = "do_chinh_xac", precision = 5, scale = 2, nullable = false)
    private BigDecimal doChinhXac;

    @Column(name = "diem_so", columnDefinition = "INT DEFAULT 0")
    private Integer diemSo = 0;

    @Column(name = "thoi_gian_tb_ms", columnDefinition = "INT DEFAULT 0")
    private Integer thoiGianTbMs = 0;

    @Column(name = "tao_luc", updatable = false)
    private LocalDateTime taoLuc;

    @PrePersist
    protected void onCreate() {
        taoLuc = LocalDateTime.now();
    }
}