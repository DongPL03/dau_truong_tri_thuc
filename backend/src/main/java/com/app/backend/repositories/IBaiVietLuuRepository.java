package com.app.backend.repositories;

import com.app.backend.models.BaiVietLuu;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface IBaiVietLuuRepository extends JpaRepository<BaiVietLuu, Long> {

    boolean existsByBaiViet_IdAndNguoiDung_Id(Long baiVietId, Long nguoiDungId);

    Optional<BaiVietLuu> findByBaiViet_IdAndNguoiDung_Id(Long baiVietId, Long nguoiDungId);

    void deleteByBaiViet_IdAndNguoiDung_Id(Long baiVietId, Long nguoiDungId);

    Page<BaiVietLuu> findByNguoiDung_IdOrderByTaoLucDesc(Long nguoiDungId, Pageable pageable);

    long countByNguoiDung_Id(Long nguoiDungId);

    void deleteAllByBaiViet_Id(Long baiVietId);
}
