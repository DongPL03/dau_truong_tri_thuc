package com.app.backend.models;

import com.app.backend.models.enums.CheDoHienThi;
import com.app.backend.models.enums.TrangThaiBoCauHoi;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

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
    @JoinColumn(name = "chu_de_id", nullable = false)
    private ChuDe chuDe;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "tao_boi_id", nullable = false)
    private NguoiDung taoBoi;

    @Enumerated(EnumType.STRING)
    @Column(name = "che_do_hien_thi", columnDefinition = "ENUM('PUBLIC','PRIVATE') DEFAULT 'PUBLIC'")
    private CheDoHienThi cheDoHienThi = CheDoHienThi.PUBLIC;

    @Enumerated(EnumType.STRING)
    @Column(name = "trang_thai", columnDefinition = "ENUM('CHO_DUYET','DA_DUYET','TU_CHOI') DEFAULT 'DA_DUYET'")
    private TrangThaiBoCauHoi trangThai = TrangThaiBoCauHoi.DA_DUYET;

    @Column(name = "ly_do_tu_choi")
    private String lyDoTuChoi;

}
