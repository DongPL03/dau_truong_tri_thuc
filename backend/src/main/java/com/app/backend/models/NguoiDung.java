package com.app.backend.models;

import jakarta.persistence.*;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.LocalDateTime;

@Entity
@Table(name = "nguoi_dung")
@Data
@EqualsAndHashCode(callSuper = false)
public class NguoiDung {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "ten_dang_nhap", length = 50, nullable = false, unique = true)
    private String tenDangNhap;

    @Column(name = "email", length = 100, nullable = false, unique = true)
    private String email;

    @Column(name = "mat_khau", nullable = false)
    private String matKhau;

    @Column(name = "ten_hien_thi", length = 100)
    private String tenHienThi;

    @Column(name = "avatar_url")
    private String avatarUrl;

    @Enumerated(EnumType.STRING)
    @Column(name = "vai_tro", columnDefinition = "ENUM('USER','ADMIN') DEFAULT 'USER'")
    private VaiTro vaiTro = VaiTro.USER;

    @Enumerated(EnumType.STRING)
    @Column(name = "trang_thai", columnDefinition = "ENUM('ONLINE','OFFLINE','BANNED') DEFAULT 'OFFLINE'")
    private TrangThaiNguoiDung trangThai = TrangThaiNguoiDung.OFFLINE;

    @Column(name = "is_deleted", columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isDeleted = false;

    @Column(name = "last_login_at")
    private LocalDateTime lastLoginAt;

    @Column(name = "tao_luc", updatable = false)
    private LocalDateTime taoLuc;

    @Column(name = "cap_nhat_luc")
    private LocalDateTime capNhatLuc;

    @PrePersist
    protected void onCreate() {
        taoLuc = LocalDateTime.now();
        capNhatLuc = LocalDateTime.now();
    }

    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = LocalDateTime.now();
    }
}

