package com.app.backend.models;

import com.app.backend.models.enums.VaiTroPhongChat;
import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "thanh_vien_phong_chat", 
    uniqueConstraints = @UniqueConstraint(columnNames = {"phong_chat_id", "nguoi_dung_id"}))
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ThanhVienPhongChat {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "phong_chat_id", nullable = false)
    private PhongChat phongChat;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    private NguoiDung nguoiDung;

    @Enumerated(EnumType.STRING)
    @Column(name = "vai_tro")
    @Builder.Default
    private VaiTroPhongChat vaiTro = VaiTroPhongChat.THANH_VIEN;

    @Column(name = "biet_danh", length = 50)
    private String bietDanh; // Biệt danh trong phòng chat

    @Column(name = "tham_gia_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant thamGiaLuc;

    @Column(name = "doc_cuoi_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant docCuoiLuc; // Thời điểm đọc tin nhắn cuối cùng

    @Column(name = "da_tat_thong_bao")
    @Builder.Default
    private Boolean daTatThongBao = false;

    @Column(name = "da_ghim")
    @Builder.Default
    private Boolean daGhim = false; // Ghim phòng chat lên đầu

    @Column(name = "da_roi")
    @Builder.Default
    private Boolean daRoi = false; // Đã rời nhóm

    @Column(name = "roi_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant roiLuc;

    @PrePersist
    protected void onCreate() {
        thamGiaLuc = Instant.now();
        docCuoiLuc = Instant.now();
    }
}
