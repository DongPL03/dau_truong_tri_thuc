package com.app.backend.models;

import com.app.backend.models.constant.LoaiBaoCao;
import com.app.backend.models.constant.TrangThaiBaoCao;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "bao_cao")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BaoCao {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "bai_viet_id")
    private BaiViet baiViet; // Nullable - báo cáo bài viết

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "binh_luan_id")
    private BinhLuan binhLuan; // Nullable - báo cáo bình luận

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_bao_cao_id", nullable = false)
    private NguoiDung nguoiBaoCao;

    @Enumerated(EnumType.STRING)
    @Column(name = "loai_bao_cao", length = 20, nullable = false)
    private LoaiBaoCao loaiBaoCao;

    @Column(name = "chi_tiet", length = 1000)
    private String chiTiet;

    @Enumerated(EnumType.STRING)
    @Column(name = "trang_thai", length = 20)
    @Builder.Default
    private TrangThaiBaoCao trangThai = TrangThaiBaoCao.PENDING;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "xu_ly_boi_id")
    private NguoiDung xuLyBoi; // Admin xử lý

    @Column(name = "ghi_chu_xu_ly", length = 500)
    private String ghiChuXuLy;

    @Column(name = "tao_luc")
    private Instant taoLuc;

    @Column(name = "xu_ly_luc")
    private Instant xuLyLuc;

    @PrePersist
    protected void onCreate() {
        taoLuc = Instant.now();
    }
}
