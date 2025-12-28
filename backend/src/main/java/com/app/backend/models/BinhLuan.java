package com.app.backend.models;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "binh_luan")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BinhLuan {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "bai_viet_id", nullable = false)
    private BaiViet baiViet;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_binh_luan_id", nullable = false)
    private NguoiDung nguoiBinhLuan;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "binh_luan_cha_id")
    private BinhLuan binhLuanCha; // Reply to comment (2-level)

    @Column(name = "noi_dung", columnDefinition = "TEXT", nullable = false)
    private String noiDung;

    @Column(name = "luot_thich")
    @Builder.Default
    private Integer luotThich = 0;

    @Column(name = "bi_an")
    @Builder.Default
    private Boolean biAn = false;

    @Column(name = "tao_luc")
    private Instant taoLuc;

    @Column(name = "cap_nhat_luc")
    private Instant capNhatLuc;

    // Replies (child comments)
    @OneToMany(mappedBy = "binhLuanCha", cascade = CascadeType.ALL, orphanRemoval = true)
    @Builder.Default
    private List<BinhLuan> replies = new ArrayList<>();

    @PrePersist
    protected void onCreate() {
        taoLuc = Instant.now();
        capNhatLuc = Instant.now();
    }

    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = Instant.now();
    }
}
