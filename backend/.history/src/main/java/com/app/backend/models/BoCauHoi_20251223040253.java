package com.app.backend.models;

import com.app.backend.models.constant.LoaiSuDungBoCauHoi;
import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.Formula;
import org.hibernate.annotations.Where;

@Entity
@Table(name = "bo_cau_hoi")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BoCauHoi extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "tieu_de", length = 100, nullable = false)
    private String tieuDe;

    @Column(name = "mo_ta")
    private String moTa;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "chu_de_id", nullable = false)
    private ChuDe chuDe;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "tao_boi_id", nullable = false)
    private NguoiDung taoBoi;

    @Column(name = "che_do_hien_thi")
    private String cheDoHienThi;

    @Column(name = "trang_thai")
    private String trangThai;

    @Column(name = "is_chinh_thuc")
    private Boolean isOfficial = false;

    @Column(name = "can_mo_khoa", columnDefinition = "TINYINT(1) DEFAULT 0")
    private Boolean canMoKhoa = false;   // false = free, true = phải unlock

    @Column(name = "gia_mo_khoa")
    @JsonProperty("gia_mo_khoa")
    private Long giaMoKhoa = 0L;

    /**
     * User muốn tạo bộ câu hỏi trả phí hay không
     * true = muốn tạo trả phí (admin sẽ set giá khi duyệt)
     * false = muốn tạo miễn phí
     */
    @Column(name = "muon_tao_tra_phi", columnDefinition = "TINYINT(1) DEFAULT 0")
    @JsonProperty("muon_tao_tra_phi")
    private Boolean muonTaoTraPhi = false;

    @Column(name = "loai_su_dung", columnDefinition = "ENUM('PRACTICE_ONLY','RANKED_ONLY','CASUAL_ONLY','COURSE_ONLY')")
    @JsonProperty("loai_su_dung")
    private String loaiSuDung;

    @Column(name = "ly_do_tu_choi")
    private String lyDoTuChoi;

    @Column(name = "is_xoa")
    @Where(clause = "is_xoa = false")
    private Boolean isXoa = false;

    @Formula("(SELECT COUNT(ch.id) FROM cau_hoi ch WHERE ch.bo_cau_hoi_id = id)")
    private int soCauHoi;
}
