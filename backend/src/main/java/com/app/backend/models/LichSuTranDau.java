package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "lich_su_tran_dau", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"tran_dau_id", "nguoi_dung_id"})
})
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LichSuTranDau {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "tran_dau_id", nullable = false)
    private TranDau tranDau;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    private NguoiDung nguoiDung;

    @Column(name = "combo_toi_da")
    private Integer maxCombo;

    @Column(name = "tong_diem", nullable = false)
    private Integer tongDiem;

    @Column(name = "so_cau_dung", nullable = false)
    private Integer soCauDung;

    @Column(name = "tong_thoi_gian_ms")
    private Integer tongThoiGianMs;

    @Column(name = "xep_hang")
    private Integer xepHang;

    @Column(name = "hoan_thanh_luc", updatable = false)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant hoanThanhLuc;

    @PrePersist
    protected void onCreate() {
        hoanThanhLuc = Instant.now();
    }
}