package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "khoa_hoc")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class KhoaHoc extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "tieu_de", length = 200, nullable = false)
    private String tieuDe;

    @Column(name = "mo_ta", columnDefinition = "TEXT")
    private String moTa;

    @Column(name = "hinh_anh")
    private String hinhAnh;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "chu_de_id", nullable = false)
    private ChuDe chuDe;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "tao_boi_id", nullable = false)
    private NguoiDung taoBoi;

    @Column(name = "trang_thai", length = 50)
    private String trangThai; // DRAFT, PUBLISHED, ARCHIVED

    @Column(name = "gia_mo_khoa")
    @JsonProperty("gia_mo_khoa")
    private Long giaMoKhoa = 0L;

    @Column(name = "thu_tu")
    private Integer thuTu = 0;

    @Column(name = "is_xoa")
    private Boolean isXoa = false;

    @OneToMany(mappedBy = "khoaHoc", cascade = CascadeType.ALL, orphanRemoval = true)
    @OrderBy("thuTu ASC")
    @Builder.Default
    private List<KhoaHocBoCauHoi> danhSachBoCauHoi = new ArrayList<>();
}

