package com.app.backend.models;

import jakarta.persistence.*;
import lombok.Data;

import java.time.LocalDateTime;

@Entity
@Table(name = "tin_nhan")
@Data
public class TinNhan {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "tran_dau_id")
    private TranDau tranDau;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "gui_boi_id", nullable = false)
    private NguoiDung guiBoi;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nhan_boi_id")
    private NguoiDung nhanBoi;

    @Lob
    @Column(name = "noi_dung", nullable = false)
    private String noiDung;

    @Column(name = "gui_luc", updatable = false)
    private LocalDateTime guiLuc;

    @PrePersist
    protected void onCreate() {
        guiLuc = LocalDateTime.now();
    }
}