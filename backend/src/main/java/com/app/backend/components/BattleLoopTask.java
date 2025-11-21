package com.app.backend.components;

import com.app.backend.models.BattleState;
import com.app.backend.models.CauHoi;
import com.app.backend.models.TranDau;
import com.app.backend.models.constant.TrangThaiTranDau;
import com.app.backend.repositories.ITranDauRepository;
import com.app.backend.services.trandau.ITranDauService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.List;

@Component
@RequiredArgsConstructor
public class BattleLoopTask {

    private final BattleStateManager battleStateManager;
    private final ITranDauRepository tranDauRepository;
    @Lazy
    @Autowired
    private ITranDauService tranDauService;  // ✅ TRÌ HOÃN KHỞI TẠO - cắt vòng lặp


    private final BattleWsPublisher wsPublisher;

    @Async
    public void runAutoLoop(Long tranDauId, int secondsPerQuestion) {
        BattleState state = battleStateManager.get(tranDauId);
        if (state == null) return;
        if (state.isAutoLoopRunning()) return;

        state.setAutoLoopRunning(true);
        if (state.getSecondsPerQuestion() <= 0) {
            state.setSecondsPerQuestion(secondsPerQuestion);
        }
        if (state.getStartTime() == null) {
            state.setStartTime(Instant.now());
        }
        battleStateManager.save(state);

        TranDau td = tranDauRepository.findById(tranDauId).orElse(null);
        if (td == null) {
            state.setAutoLoopRunning(false);
            battleStateManager.save(state);
            return;
        }

        try {
            int preCountdownSeconds = 10;
            // 🔔 Thông báo trận đấu bắt đầu
            wsPublisher.publishBattleStarted(
                    tranDauId,
                    td.getTenPhong() != null ? td.getTenPhong() : ("Phòng #" + tranDauId),
                    state.getStartTime(),
                    state.getDanhSachCauHoi().size(),
                    state.getSecondsPerQuestion(),
                    preCountdownSeconds
            );

            try {
                Thread.sleep(preCountdownSeconds * 1000L);
            } catch (InterruptedException ie) {
                Thread.currentThread().interrupt();
                return;
            }


            List<CauHoi> cauHoiList = state.getDanhSachCauHoi();
            for (int i = 0; i < cauHoiList.size(); i++) {
                // luôn lấy state mới nhất
                BattleState latest = battleStateManager.get(tranDauId);
                if (latest == null || !latest.isAutoLoopRunning()) {
                    break; // có ai đó stop loop
                }

                latest.setCurrentQuestionIndex(i);
                latest.setCurrentQuestionStart(Instant.now());
                battleStateManager.save(latest);

                CauHoi q = cauHoiList.get(i);
                wsPublisher.publishNewQuestion(tranDauId, i, q, latest.getSecondsPerQuestion());

                try {
                    Thread.sleep(latest.getSecondsPerQuestion() * 1000L);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    break;
                }

                BattleState afterSleep = battleStateManager.get(tranDauId);
                if (afterSleep == null || !afterSleep.isAutoLoopRunning()) break;

                // 1. Gửi đáp án (REVEAL)
                String dapAnDung = String.valueOf(q.getDapAnDung());
                String giaiThich = q.getGiaiThich();
                wsPublisher.publishAnswerReveal(tranDauId, q.getId(), dapAnDung, giaiThich);

                // 🔥 FIX: THÊM THỜI GIAN CHỜ ĐỂ NGƯỜI DÙNG ĐỌC ĐÁP ÁN (ví dụ 5 giây)
                try {
                    // Thời gian nghỉ giữa các hiệp
                    int timeBreak = 5000; // 5 giây
                    System.out.println("--- Nghỉ " + timeBreak + "ms để xem đáp án ---");
                    Thread.sleep(timeBreak);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }

            // ❗❗ HẾT CÂU HỎI → CHỈ GỌI SERVICE, KHÔNG TỰ SET FINISHED
            Long hostId = (td.getChuPhong() != null) ? td.getChuPhong().getId() : null;
            System.out.println(">>> [LOOP] Hết câu hỏi, gọi finishBattle(auto), tranDauId=" + tranDauId);
            tranDauService.finishBattle(tranDauId, hostId, true);

        } catch (Exception e) {
            System.err.println("❌ Lỗi trong BattleLoopTask: " + e.getMessage());
            e.printStackTrace();
        } finally {
            state.setAutoLoopRunning(false);
            battleStateManager.save(state);
            // ❌ KHÔNG remove state ở đây, đã có finishBattle xử lý
            // battleStateManager.remove(tranDauId);
        }
    }

}
