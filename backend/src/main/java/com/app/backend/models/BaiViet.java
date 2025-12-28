package com.app.backend.models;

import com.app.backend.models.constant.LoaiBaiViet;
import com.app.backend.models.constant.TrangThaiBaiViet;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "bai_viet")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BaiViet {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_dang_id", nullable = false)
    private NguoiDung nguoiDang;

    @Column(name = "tieu_de", nullable = false, length = 255)
    private String tieuDe;

    @Column(name = "noi_dung", columnDefinition = "TEXT")
    private String noiDung;

    @Enumerated(EnumType.STRING)
    @Column(name = "loai_bai", length = 20)
    @Builder.Default
    private LoaiBaiViet loaiBai = LoaiBaiViet.THAO_LUAN;

    @Enumerated(EnumType.STRING)
    @Column(name = "trang_thai", length = 20)
    @Builder.Default
    private TrangThaiBaiViet trangThai = TrangThaiBaiViet.PENDING;

    @Column(name = "luot_xem")
    @Builder.Default
    private Integer luotXem = 0;

    @Column(name = "luot_thich")
    @Builder.Default
    private Integer luotThich = 0;

    @Column(name = "luot_binh_luan")
    @Builder.Default
    private Integer luotBinhLuan = 0;

    @Column(name = "ghim")
    @Builder.Default
    private Boolean ghim = false;

    @Column(name = "tao_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant taoLuc;

    @Column(name = "cap_nhat_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant capNhatLuc;

    @Column(name = "duyet_luc")
    private Instant duyetLuc;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "duyet_boi_id")
    private NguoiDung duyetBoi;

    // Relationships
    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "bai_viet_tag",
            joinColumns = @JoinColumn(name = "bai_viet_id"),
            inverseJoinColumns = @JoinColumn(name = "tag_id")
    )
    @Builder.Default
    private List<Tag> tags = new ArrayList<>();

    @OneToMany(mappedBy = "baiViet", cascade = CascadeType.ALL, orphanRemoval = true)
    @Builder.Default
    private List<HinhAnhBaiViet> hinhAnhs = new ArrayList<>();

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
