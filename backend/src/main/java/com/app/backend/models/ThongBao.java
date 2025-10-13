package com.app.backend.models;

import jakarta.persistence.*;
import lombok.Data;

import java.time.LocalDateTime;

@Entity
@Table(name = "thong_bao")
@Data
public class ThongBao {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_gui_id", nullable = false)
    private NguoiDung nguoiGui;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_nhan_id", nullable = false)
    private NguoiDung nguoiNhan;

    @Enumerated(EnumType.STRING)
    @Column(name = "loai", columnDefinition = "ENUM('FRIEND_REQUEST','BATTLE_INVITE','SYSTEM')")
    private LoaiThongBao loai;

    @Column(name = "noi_dung")
    private String noiDung;

    @Column(name = "metadata", columnDefinition = "JSON")
    private String metadata; // Sử dụng String để lưu chuỗi JSON

    @Column(name = "da_doc", columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean daDoc = false;

    @Column(name = "tao_luc", updatable = false)
    private LocalDateTime taoLuc;

    @PrePersist
    protected void onCreate() {
        taoLuc = LocalDateTime.now();
    }
}


