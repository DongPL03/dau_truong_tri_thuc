package com.app.backend.models;

import com.app.backend.models.enums.LoaiTinNhan;
import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "tin_nhan_phong_chat", indexes = {
    @Index(name = "idx_tin_nhan_phong_chat", columnList = "phong_chat_id, gui_luc DESC"),
    @Index(name = "idx_tin_nhan_gui_boi", columnList = "gui_boi_id")
})
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TinNhanPhongChat {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "phong_chat_id", nullable = false)
    private PhongChat phongChat;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "gui_boi_id", nullable = false)
    private NguoiDung guiBoi;

    @Enumerated(EnumType.STRING)
    @Column(name = "loai", nullable = false)
    @Builder.Default
    private LoaiTinNhan loai = LoaiTinNhan.VAN_BAN;

    @Column(name = "noi_dung", columnDefinition = "TEXT")
    private String noiDung;

    @Column(name = "url_media")
    private String urlMedia; // URL cho ảnh, file, audio

    @Column(name = "ten_file")
    private String tenFile; // Tên file gốc

    @Column(name = "kich_thuoc_file")
    private Long kichThuocFile; // Kích thước file (bytes)

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "tra_loi_cho_id")
    private TinNhanPhongChat traLoiCho; // Reply to message

    @Column(name = "gui_luc", updatable = false)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant guiLuc;

    @Column(name = "chinh_sua_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant chinhSuaLuc;

    @Column(name = "da_xoa")
    @Builder.Default
    private Boolean daXoa = false;

    @Column(name = "xoa_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant xoaLuc;

    @Column(name = "da_ghim")
    @Builder.Default
    private Boolean daGhim = false;

    @PrePersist
    protected void onCreate() {
        guiLuc = Instant.now();
    }
}
