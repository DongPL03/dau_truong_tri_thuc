package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
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

    @Column(name = "ly_do_tu_choi")
    private String lyDoTuChoi;

    @Column(name = "is_delete")
    @Where(clause = "is_delete = false")
    private Boolean isDelete = false;

    @Formula("(SELECT COUNT(ch.id) FROM cau_hoi ch WHERE ch.bo_cau_hoi_id = id)")
    private int soCauHoi;
}
