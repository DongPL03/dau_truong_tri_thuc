package com.app.backend.models;

import jakarta.persistence.*;
import lombok.Data;

import java.time.LocalDateTime;

@Entity
@Table(name = "tran_dau")
@Data
public class TranDau {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "bo_cau_hoi_id", nullable = false)
    private BoCauHoi boCauHoi;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "chu_phong_id", nullable = false)
    private NguoiDung chuPhong;

    @Column(name = "ma_phong", length = 10, nullable = false, unique = true)
    private String maPhong;

    @Enumerated(EnumType.STRING)
    @Column(name = "trang_thai", columnDefinition = "ENUM('PENDING','ONGOING','FINISHED') DEFAULT 'PENDING'")
    private TrangThaiTranDau trangThai = TrangThaiTranDau.PENDING;

    @Column(name = "cong_khai", columnDefinition = "BOOLEAN DEFAULT TRUE")
    private Boolean congKhai = true;

    @Column(name = "ma_pin", length = 10)
    private String maPin;

    @Column(name = "gioi_han_nguoi_choi", columnDefinition = "INT DEFAULT 5")
    private Integer gioiHanNguoiChoi = 5;

    @Column(name = "gioi_han_thoi_gian_cau_giay", columnDefinition = "INT DEFAULT 15")
    private Integer gioiHanThoiGianCauGiay = 15;

    @Enumerated(EnumType.STRING)
    @Column(name = "luat_tinh_diem", columnDefinition = "ENUM('BASIC','SPEED_BONUS') DEFAULT 'SPEED_BONUS'")
    private LuatTinhDiem luatTinhDiem = LuatTinhDiem.SPEED_BONUS;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "winner_id")
    private NguoiDung winner;

    @Column(name = "tao_luc", updatable = false)
    private LocalDateTime taoLuc;

    @Column(name = "bat_dau_luc")
    private LocalDateTime batDauLuc;

    @Column(name = "ket_thuc_luc")
    private LocalDateTime ketThucLuc;

    @PrePersist
    protected void onCreate() {
        taoLuc = LocalDateTime.now();
    }
}