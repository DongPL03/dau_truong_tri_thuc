package com.app.backend.repositories;

import com.app.backend.models.BaoCao;
import com.app.backend.models.constant.LoaiBaoCao;
import com.app.backend.models.constant.TrangThaiBaoCao;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface IBaoCaoRepository extends JpaRepository<BaoCao, Long> {

    Page<BaoCao> findByTrangThaiOrderByTaoLucDesc(TrangThaiBaoCao trangThai, Pageable pageable);

    Page<BaoCao> findAllByOrderByTaoLucDesc(Pageable pageable);

    // Lấy báo cáo của user
    Page<BaoCao> findByNguoiBaoCao_IdOrderByTaoLucDesc(Long userId, Pageable pageable);

    // Kiểm tra user đã báo cáo bài viết chưa
    boolean existsByBaiViet_IdAndNguoiBaoCao_Id(Long baiVietId, Long nguoiBaoCaoId);

    // Kiểm tra user đã báo cáo comment chưa
    boolean existsByBinhLuan_IdAndNguoiBaoCao_Id(Long binhLuanId, Long nguoiBaoCaoId);

    // Đếm số báo cáo theo trạng thái
    long countByTrangThai(TrangThaiBaoCao trangThai);

    // Đếm số báo cáo theo loại
    long countByLoaiBaoCao(LoaiBaoCao loaiBaoCao);

    // Lấy báo cáo của bài viết
    Page<BaoCao> findByBaiViet_IdOrderByTaoLucDesc(Long baiVietId, Pageable pageable);

    // Lấy báo cáo của comment
    Page<BaoCao> findByBinhLuan_IdOrderByTaoLucDesc(Long binhLuanId, Pageable pageable);
}
