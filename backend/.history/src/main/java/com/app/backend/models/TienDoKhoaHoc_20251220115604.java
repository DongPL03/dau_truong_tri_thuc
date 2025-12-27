package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.Instant;

@Entity
@Table(name = "tien_do_khoa_hoc",
        uniqueConstraints = {
                @UniqueConstraint(columnNames = {"nguoi_dung_id", "khoa_hoc_id"})
        })
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TienDoKhoaHoc extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    private NguoiDung nguoiDung;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "khoa_hoc_id", nullable = false)
    private KhoaHoc khoaHoc;

    @Column(name = "so_bo_da_hoan_thanh")
    private Integer soBoDaHoanThanh = 0;

    @Column(name = "tong_so_bo")
    private Integer tongSoBo = 0;

    @Column(name = "phan_tram_hoan_thanh", precision = 5, scale = 2)
    private BigDecimal phanTramHoanThanh = BigDecimal.ZERO;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "bo_cau_hoi_hien_tai_id")
    private BoCauHoi boCauHoiHienTai;

    @Column(name = "trang_thai", length = 50)
    private String trangThai; // CHUA_BAT_DAU, DANG_HOC, HOAN_THANH, TAM_DUNG

    @Column(name = "ngay_bat_dau")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ngayBatDau;

    @Column(name = "ngay_hoan_thanh")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ngayHoanThanh;
}

