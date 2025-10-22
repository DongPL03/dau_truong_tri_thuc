package com.app.backend.models;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "tra_loi_tran_dau", uniqueConstraints = {
        @UniqueConstraint(name = "uq_traloi_unique", columnNames = {"tran_dau_id", "nguoi_dung_id", "cau_hoi_id"})
})
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TraLoiTranDau {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "tran_dau_id", nullable = false)
    private TranDau tranDau;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    private NguoiDung nguoiDung;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "cau_hoi_id", nullable = false)
    private CauHoi cauHoi;

    @Column(name = "lua_chon", length = 1, nullable = false)
    private Character luaChon;

    @Column(name = "dung_hay_sai", nullable = false)
    private Boolean dungHaySai;

    @Column(name = "thoi_gian_ms")
    private Integer thoiGianMs;

    @Column(name = "tra_loi_luc", updatable = false)
    private LocalDateTime traLoiLuc;

    @PrePersist
    protected void onCreate() {
        traLoiLuc = LocalDateTime.now();
    }
}