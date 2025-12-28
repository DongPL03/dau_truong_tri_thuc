package com.app.backend.repositories;

import com.app.backend.models.LuotThich;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ILuotThichRepository extends JpaRepository<LuotThich, Long> {

    boolean existsByBaiViet_IdAndNguoiDung_Id(Long baiVietId, Long nguoiDungId);

    Optional<LuotThich> findByBaiViet_IdAndNguoiDung_Id(Long baiVietId, Long nguoiDungId);

    void deleteByBaiViet_IdAndNguoiDung_Id(Long baiVietId, Long nguoiDungId);

    long countByBaiViet_Id(Long baiVietId);

    void deleteAllByBaiViet_Id(Long baiVietId);
}
