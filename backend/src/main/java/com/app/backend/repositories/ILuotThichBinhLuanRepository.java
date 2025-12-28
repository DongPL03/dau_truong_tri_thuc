package com.app.backend.repositories;

import com.app.backend.models.LuotThichBinhLuan;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ILuotThichBinhLuanRepository extends JpaRepository<LuotThichBinhLuan, Long> {

    boolean existsByBinhLuan_IdAndNguoiDung_Id(Long binhLuanId, Long nguoiDungId);

    Optional<LuotThichBinhLuan> findByBinhLuan_IdAndNguoiDung_Id(Long binhLuanId, Long nguoiDungId);

    void deleteByBinhLuan_IdAndNguoiDung_Id(Long binhLuanId, Long nguoiDungId);

    long countByBinhLuan_Id(Long binhLuanId);
}
